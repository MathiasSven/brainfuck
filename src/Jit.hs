{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Jit where

import           CodeGen.X86
import           Control.Monad (replicateM_)
import           Data.List     (group)
import           Foreign       (FunPtr, Ptr, Word16, Word8, callocBytes)

import           Parser

{- HLINT ignore "Redundant bracket" -} -- https://github.com/JustusAdam/language-haskell/issues/190
foreign import ccall "static <stdio.h> &putchar" c_putchar_ptr :: FunPtr (Int -> IO ())
foreign import ccall "static <stdio.h> &getchar" c_getchar_ptr :: FunPtr (IO Int)
foreign import ccall "dynamic" withMemory :: FunPtr (Ptr a -> IO ()) -> (Ptr a) -> IO ()

instance Callable (Ptr a -> IO ()) where
  dynCCall :: FunPtr (Ptr a -> IO ()) -> Ptr a -> IO ()
  dynCCall = withMemory

type BrainfuckA = [ExprA]

data ExprA
  = OpA Op Int
  | LoopA BrainfuckA
  deriving Show

jitCompile :: BrainfuckA -> Code
jitCompile bfa = go bfa >> ret
  where
    fromInt2W8 i = fromIntegral $ fromIntegral @_ @Word8 i
    go = mapM_ \case
      OpA OpInc    i -> add (addr8 di) (fromInt2W8 i)
      OpA OpDec    i -> sub (addr8 di) (fromInt2W8 i)
      OpA OpLeft   i -> sub di (fromInt2W8 i)
      OpA OpRight  i -> add di (fromInt2W8 i)
      OpA OpOutput i -> replicateM_ (fromInt2W8 i) do
        push rdi
        push rbp
        mov di (addr16 di)
        callFun rbp c_putchar_ptr
        pop rbp
        pop rdi
      OpA OpInput i -> replicateM_ (fromIntegral i) do
        push rdi
        push rbp
        callFun rbp c_getchar_ptr
        pop rbp
        pop rdi
        mov (addr16 di) ax
      LoopA bf -> mdo
        cmp (addr8 di) 0
        j Z l2
        l1 <- label
        go bf
        cmp (addr8 di) 0
        j NZ l1
        l2 <- label
        return ()

deriving instance Eq Expr -- Orphan
deriving instance Eq Op   -- Orphan

groupOps :: Brainfuck -> BrainfuckA
groupOps = fmap accum . group
  where
    accum :: Brainfuck -> ExprA
    accum expr@((Op op):_) = OpA op $ length expr
    accum ((Loop bf):_)    = LoopA $ groupOps bf
    accum []               = error "Cannot accum empty list"

runBrainfuck :: Brainfuck -> IO ()
runBrainfuck bf = do
  let bfa = groupOps bf
      bfcode = jitCompile bfa

      code :: Ptr () -> IO ()
      code = compile bfcode

  memPtr <- callocBytes (fromIntegral (maxBound @Word16) * 2)
  code memPtr

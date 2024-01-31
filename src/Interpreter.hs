{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LexicalNegation     #-}

module Interpreter where

import           Control.Monad               (forM_, join, liftM2, unless)
import           Data.ByteString.Internal    (c2w, w2c)
import           Data.Function               (fix, (&))
import           Data.Primitive              (Prim)
import           Data.Primitive.PrimVar      (modifyPrimVar, newPrimVar,
                                              readPrimVar)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Data.Word                   (Word8)

import           Parser


runBrainfuck :: forall a. (Integral a, Bounded a, Prim a) => Brainfuck -> IO ()
runBrainfuck bf = do
  dataPointer <- newPrimVar (0 :: a)
  arr <- MV.replicate @_ @Word8 (fromIntegral (maxBound @a) + 1) 0

  let readPV = fromIntegral <$> readPrimVar dataPointer
      modifyPV = modifyPrimVar dataPointer

  bf & fix \evalLoop bf' ->
    forM_ bf' \case
      Op OpInc    -> readPV >>= MV.modify arr (+ 1)
      Op OpDec    -> readPV >>= MV.modify arr (- 1)
      Op OpLeft   -> modifyPV (- 1)
      Op OpRight  -> modifyPV (+ 1)
      Op OpOutput -> readPV >>= MV.read arr >>= putChar . w2c
      Op OpInput  -> join $ liftM2 (MV.write arr) readPV (c2w <$> getChar)
      Loop bf''   -> fix \recurse -> do
        state <- readPV >>= MV.read arr
        unless (state == 0) (evalLoop bf'' >> recurse)

  putChar '\n'

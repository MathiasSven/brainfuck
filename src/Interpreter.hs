{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LexicalNegation     #-}

module Interpreter where

import           Control.Monad               (forM_, join, liftM2, unless, void)
import           Data.ByteString.Internal    (c2w, w2c)
import           Data.Function               (fix, (&))
import           Data.Primitive              (Prim)
import           Data.Primitive.PrimVar      (modifyPrimVar, newPrimVar,
                                              readPrimVar, PrimVar)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Data.Word                   (Word8)

import           Parser


type BFPointer a = (Integral a, Bounded a, Prim a)

data BFState m a = BFState
  { dataPointer :: PrimVar (MV.PrimState m) a
  , arr         :: MV.MVector (MV.PrimState m) Word8
  }

mkBFState :: forall a m. (MV.PrimMonad m, BFPointer a) => m (BFState m a)
mkBFState = BFState
        <$> newPrimVar (0 :: a)
        <*> MV.replicate @_ @Word8 (fromIntegral (maxBound @a) + 1) 0

runBrainfuck :: forall a. BFPointer a => Brainfuck -> IO ()
runBrainfuck bf = mkBFState @a >>= void . runBrainfuckWithState bf

runBrainfuckWithState :: BFPointer a => Brainfuck -> BFState IO a -> IO (BFState IO a)
runBrainfuckWithState bf st = do
  let bfstate@BFState{dataPointer, arr} = st

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
  return bfstate

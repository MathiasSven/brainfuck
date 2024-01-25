{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LexicalNegation     #-}

module Main where

import           Control.Monad            (forM_, join, liftM2, unless)
import           Data.ByteString.Internal (c2w, w2c)
import           Data.Function            (fix, (&))
import           Data.Functor             (($>), (<&>))
import           Data.IORef               (modifyIORef, newIORef, readIORef)
import           Data.Text                (Text)
import qualified Data.Text.IO             as T
import qualified Data.Vector.Mutable      as MV
import           Data.Word                (Word16, Word8)
import           System.Environment       (getArgs)
import           Text.Parsec              (between, char, choice, noneOf, parse,
                                           sepEndBy, skipMany, (<|>))
import           Text.Parsec.Text         (Parser)


type Brainfuck = [Expr]

data Expr
  = Op Op
  | Loop Brainfuck
  deriving Show

data Op
  = OpInc
  | OpDec
  | OpLeft
  | OpRight
  | OpOutput
  | OpInput
  deriving (Show, Enum)

opChar :: Op -> Char
opChar OpInc    = '+'
opChar OpDec    = '-'
opChar OpLeft   = '<'
opChar OpRight  = '>'
opChar OpOutput = '.'
opChar OpInput  = ','

brainfuckParser :: Parser Brainfuck
brainfuckParser = ignore *> sepEndBy (Op <$> choice opParsers <|> loopParser) ignore
  where
    opParsers = [OpInc ..] <&> \t -> char (opChar t) $> t
    loopParser = Loop <$> between (char '[') (char ']') brainfuckParser
    ignore = skipMany $ noneOf ((opChar <$> [OpInc ..]) ++ ['[', ']'])

runBrainfuck :: forall a. (Num a, Integral a, Bounded a) => Brainfuck -> IO ()
runBrainfuck bf = do
  dataPointer <- newIORef (0 :: a)
  arr <- MV.replicate @_ @Word8 (fromIntegral (maxBound @a) + 1) 0

  let readRef = fromIntegral <$> readIORef dataPointer
      modifyRef = modifyIORef dataPointer

  bf & fix \evalLoop bf' ->
    forM_ bf' \case
      Op OpInc    -> readRef >>= MV.modify arr (+ 1)
      Op OpDec    -> readRef >>= MV.modify arr (- 1)
      Op OpLeft   -> modifyRef (- 1)
      Op OpRight  -> modifyRef (+ 1)
      Op OpOutput -> readRef >>= MV.read arr >>= putChar . w2c
      Op OpInput  -> join $ liftM2 (MV.write arr) readRef (c2w <$> getChar)
      Loop bf''   -> fix \recurse -> do
        state <- readRef >>= MV.read arr
        unless (state == 0) (evalLoop bf'' >> recurse)

  putChar '\n'

main :: IO ()
main = do
  [filePath] <- getArgs
  file <- T.readFile filePath
  case parse brainfuckParser filePath file of
    Left err -> print err
    Right bf -> runBrainfuck @Word16 bf

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LexicalNegation     #-}

module Main where

import           Control.Monad            (void)
import           Data.ByteString.Internal (c2w, w2c)
import           Data.Function            (fix, (&))
import           Data.Functor             (($>), (<&>))
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

runBrainfuck :: Brainfuck -> IO ()
runBrainfuck bf = do
  arr <- MV.replicate @_ @Word8 (fromIntegral (maxBound @Word16) + 1) 0

  let dataPointer = (0 :: Word16)
      w2i = fromIntegral

      evalLoop :: Word16 -> Brainfuck -> IO Word16
      evalLoop !dp = \case
        []               -> return dp
        Op OpInc    : xs -> MV.modify arr (+ 1) (w2i dp) >> evalLoop dp xs
        Op OpDec    : xs -> MV.modify arr (- 1) (w2i dp) >> evalLoop dp xs
        Op OpLeft   : xs -> evalLoop (dp - 1) xs
        Op OpRight  : xs -> evalLoop (dp + 1) xs
        Op OpOutput : xs -> MV.read arr (w2i dp) >>= putChar . w2c >> evalLoop dp xs
        Op OpInput  : xs -> (MV.write arr (w2i dp) . c2w =<< getChar) >> evalLoop dp xs
        Loop bf''   : xs ->
            dp & fix \loop !dp' -> do
              state <- MV.read arr (w2i dp')
              if state == 0 then evalLoop dp' xs else evalLoop dp' bf'' >>= loop

  void $ evalLoop dataPointer bf

  putChar '\n'

main :: IO ()
main = do
  [filePath] <- getArgs
  file <- T.readFile filePath
  case parse brainfuckParser filePath file of
    Left err -> print err
    Right bf -> runBrainfuck bf

module Parser where

import           Data.Functor     (($>), (<&>))
import           Data.Text        (Text)
import           Text.Parsec      (ParseError, between, char, choice, noneOf,
                                   parse, sepEndBy, skipMany, (<|>))
import           Text.Parsec.Text (Parser)

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

parseBrainfuck :: FilePath -> Text -> Either ParseError Brainfuck
parseBrainfuck = parse brainfuckParser

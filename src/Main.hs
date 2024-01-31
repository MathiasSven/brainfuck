{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Data.Text.IO                    as T
import           Data.Word                       (Word16)
import           System.Console.CmdArgs.Implicit hiding (Mode)
import           System.FilePath                 (dropExtension)
import           System.IO                       (BufferMode (..),
                                                  hSetBuffering, stdin, stdout)

import           Compiler                        (compileBrainfuck)
import           Interpreter                     (runBrainfuck)
import qualified Jit                             (runBrainfuck)
import           Parser                          (parseBrainfuck)


main :: IO ()
main = do
  mode <- cmdArgs bfrun
  case mode of
    Interpret{noBuffering} | noBuffering -> do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
    _ -> return ()

  file <- T.readFile $ inputPath mode
  case parseBrainfuck (inputPath mode) file of
    Left err -> print err
    Right bf -> case mode of
      Interpret{jit} | jit       -> Jit.runBrainfuck bf
                     | otherwise -> runBrainfuck @Word16 bf

      Compile{inputPath, out, dontLink} -> compileBrainfuck (not dontLink) outPath bf
        where
          outPath = if out == "" then dropExtension inputPath else out


data BfRun
  = Interpret { inputPath   :: FilePath
              , jit         :: Bool
              , noBuffering :: Bool
              }
  | Compile { inputPath :: FilePath
            , out       :: FilePath
            , dontLink  :: Bool
            }
  deriving (Show, Data)

bfrun :: BfRun
bfrun = modes [interpret &= auto, compile]
        &= versionArg [ignore]
        &= summary "The bfrun utility\n\nBrainfuck interpreter, JIT and compiler"
  where
    interpret = Interpret
      { inputPath = def &= typFile &= argPos 0
      , jit = False &= help "Use JIT Compilation"
      , noBuffering = False
      }

    compile = Compile
      { inputPath = def &= argPos 0 &= typFile
      , out = def &= help "Output file" &= typFile
      , dontLink = False &= explicit &= name "c" &= help "Compile and assemble, but do not link"
      }

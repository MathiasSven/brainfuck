{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE ViewPatterns    #-}

module Repl where

import           Control.Monad               (join, void)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Bifunctor              (Bifunctor (bimap))
import           Data.Char                   (isSpace)
import           Data.Function               (fix, (&))
import           Data.List                   (isPrefixOf)
import           Data.Primitive.PrimVar      (readPrimVar, writePrimVar)
import           Data.Text                   as T (pack, split, unpack)
import qualified Data.Vector.Unboxed.Mutable as MV
import           Data.Word                   (Word16, Word8)
import           System.Console.ANSI
import           System.Console.Haskeline
import           System.IO                   (BufferMode (NoBuffering),
                                              hSetBuffering, stdin, stdout)
import           Text.RawString.QQ           (r)
import           Text.Read                   (readMaybe)

import           Interpreter
import           Parser


showHeader :: IO ()
showHeader = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn $ unlines . drop 1 $ lines [r|
 ____            _        __            _
| __ ) _ __ __ _(_)_ __  / _|_   _  ___| | __
|  _ \| '__/ _` | | '_ \| |_| | | |/ __| |/ /
| |_) | | | (_| | | | | |  _| |_| | (__|   <
|____/|_|  \__,_|_|_| |_|_|  \__,_|\___|_|\_\
|]
  putStr "Homepage: "
  putStrLn "https://github.com/MathiasSven/brainfuck"
  putStrLn "Type ? for help\n"
  setSGR [Reset]

help :: String
help = [r|Availalbe comamnds:

  @            - Display value at current position
  @n           - Display value at position n
  set@n        - Set a value at position n (omit for current)
  go@n         - Change position to n
  pos          - Display current possition
  bounds       - Show bounds of the array
  clear        - Clear values from the array
  quit         - Exit from the REPL
  ?/help       - Show help message
|]

completionFunc :: CompletionFunc IO
completionFunc = completeWord Nothing [] completions
  where
    completions s = pure
      let (c , rel) = join bimap (filter (isPrefixOf s))
            (["pos", "bounds", "clear", "quit", "help"], ["set@", "go@"])
      in (simpleCompletion <$> c) ++ ((\str -> Completion str str False) <$> rel)

pattern Command :: String -> String
pattern Command a <- (takeWhile (not . isSpace) -> a)

runRepl :: IO ()
runRepl = do
  showHeader
  bfState <- (mkBFState @Word16)

  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  runInputT (setComplete completionFunc defaultSettings) $
    bfState & fix \loop st ->
      getInputLine "[+.]: " >>= \case
        Nothing -> return ()
        Just (Command "quit") -> return ()
        Just other -> handleInput st other >> loop st

relCommandView :: String -> Maybe (String, Maybe Word16)
relCommandView str = do
  [f, s] <- return $ split (== '@') (pack str)
  return (unpack f, readMaybe @Word16 (unpack s))

handleInput :: BFState IO Word16 -> String -> InputT IO ()
handleInput st@BFState{dataPointer, arr} = \case
  (relCommandView -> Just (f, s)) -> do
    result <- liftIO case (f, s) of
      ("", Nothing)    -> do
        dataPtr <- readPrimVar dataPointer
        value <- MV.read arr (fromIntegral dataPtr)
        return $ "Value @ here: " <> show value <> "\n"

      ("", Just n)     -> do
        value <- MV.read arr (fromIntegral n)
        return $ "Value @" <> show n <> ": " <> show value <> "\n"

      ("set", Nothing) -> do
        input <- getLine
        dataPtr <- readPrimVar dataPointer
        case readMaybe @Word8 input of
          Nothing -> return "Could not read as a number\n"
          Just v  -> do
            MV.write arr (fromIntegral dataPtr) v
            return $ "Value @ here set to: " <> show v <> "\n"

      ("set", Just n)  -> do
        input <- getLine
        case readMaybe @Word8 input of
          Nothing -> return "Could not read as a number\n"
          Just v  -> do
            MV.write arr (fromIntegral n) v
            return $ "Value @" <> show n <> " set to: " <> show v <> "\n"

      ("go", Just n)   -> do
        writePrimVar dataPointer (fromIntegral n)
        return $ "Position set to: " <> show n <> "\n"

      (t, _)           -> return $ t <> " is not a relative command\n"

    outputStrLn result

  Command "pos"    -> do
    pos <- liftIO (readPrimVar dataPointer)
    outputStrLn $ "Current position: " <> show pos <> "\n"

  Command "bounds" -> do
    let b = show (0 :: Int, MV.length arr - 1)
    outputStrLn $ "Bounds are: " <> b <> "\n"

  Command "clear"  -> do
    liftIO (MV.set arr 0)
    outputStrLn "Array cleared\n"

  Command "?"      -> outputStrLn help
  Command "help"   -> outputStrLn help

  input ->
    case parseBrainfuck "<interactive>" (pack input) of
      Left err -> outputStrLn ('\n' : show err)
      Right bf -> void (liftIO (runBrainfuckWithState bf st))

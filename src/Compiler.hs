{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Compiler where

import           CodeGen.X86         (Code, callFun, rbp)
import           Data.ByteString     as B (ByteString, hPutStr)
import           Data.FileEmbed      (embedFile)
import           Data.Text           (Text, pack, replace, strip, unpack)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Data.Word           (Word16)
import           Language.Haskell.TH (runIO)
import           System.Directory    (getPermissions, removeFile,
                                      setOwnerExecutable, setPermissions)
import           System.FilePath     ((-<.>))
import           System.IO           (hClose)
import           System.IO.Temp      (withSystemTempFile)
import           System.Process      (callProcess, readProcess)

import           Jit
import           Parser
import Control.Monad (when)


fasm :: ByteString
fasm = $(do
    fasmPathDirty <- runIO (readProcess "which" ["fasm"] [])
    embedFile (unpack . strip $ pack fasmPathDirty)
  )

fasmCompile :: [String] -> IO ()
fasmCompile args = withSystemTempFile "fasm" \file handle -> do
  B.hPutStr handle fasm
  hClose handle
  perms <- getPermissions file
  setPermissions file (setOwnerExecutable True perms)
  callProcess file args

compileBrainfuck :: Bool -> FilePath -> Brainfuck -> IO ()
compileBrainfuck link outPath bf = do
  let asmCode = toFASMCode $ jitCompile (groupOps bf)
  withSystemTempFile "bfrun-code.fasm" \file handle -> do
    T.hPutStr handle asmCode
    hClose handle
    let objFile = outPath -<.> "o"
    fasmCompile [file, objFile]
    when link do 
      callProcess "ld" [objFile, "-lc", "-o", outPath]
      removeFile objFile

toFASMCode :: Code -> Text
toFASMCode code = header <> fixup (pack mainCode) <> footer
  where
    mainCode = (unlines . init . fmap lineHandler . lines . show) padedCode

    padedCode = do
      callFun rbp c_getchar_ptr
      callFun rbp c_putchar_ptr
      code

    header = pack $ unlines
      [ "format ELF64"
      , "extrn calloc"
      , "extrn putchar"
      , "extrn exit"
      , "extrn getchar"
      , "public _start"
      , "section '.text' executable"
      , "_start:"
      , "mov rdi, " ++ show @Int (fromIntegral (maxBound @Word16) * 2)
      , "mov rsi, 1"
      , "call calloc"
      , "mov rdi, rax"
      ]

    footer = pack $ unlines
      [ "mov rdi, 0"
      , "call exit"
      ]

    lineHandler (words -> l) =
      if length l == 1
        then unwords l ++ ":"
        else unwords . drop 2 $ l

    fixup t =
      replace getCharRep "call getchar\n" $
      replace putCharRep "call putchar\n" $
      T.unlines $ drop 4 $ T.lines t
      where
        getCharRep = T.unlines . take 2 $ T.lines t
        putCharRep = T.unlines . take 2 . drop 2 $ T.lines t

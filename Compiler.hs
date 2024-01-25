{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Compiler where

import CodeGen.X86
import Foreign (nullFunPtr)


main = do
  print code

code = do
  inc $ addr8 rdi
  push rdi

write :: Code
write = do
  {- write - System Calls Manual
     https://man7.org/linux/man-pages/man2/write.2.html
     Calling convention reference: 
     https://chromium.googlesource.com/chromiumos/docs/+/HEAD/constants/syscalls.md#x86_64-64_bit
  -}
  mov rax 1       -- set syswrite
  mov arg1 1      -- arg1 is rdi, sets the file descriptor
  mov arg2 69    -- rsi is the second param
  -- mov arg2 rdi    -- rsi is the second param
  mov arg3 1      -- arg3 is rdx, sets length of the given buffer on arg2
  db [0x0f, 0x05] -- syscall
  ret

-- foo :: Int
-- foo = compile write
-- 

instance Callable () where
  dynCCall = nullFunPtr

module Register where

import Data.Array.MArray
import Data.Array.IO
import Data.Word

type RegisterFile = IOUArray Int Word32

createRegister :: IO RegisterFile
createRegister = newArray (1, 31) 0

get :: Int -> RegisterFile -> IO Word32
get n reg = if n == 0 then return 0 else readArray reg n

set :: Int -> Word32 -> RegisterFile -> IO ()
set n value reg = writeArray reg n value

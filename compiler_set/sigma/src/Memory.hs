module Memory where

import Data.Array.MArray
import Data.Array.IO
import Data.Word

type Memory = IOUArray Int Word32

memorySize = 0x200000

createMemory :: IO Memory
createMemory = newArray (0, memorySize-1) 0

get :: Int -> Memory -> IO Word32
get n reg = readArray reg (n `mod` memorySize)

set :: Int -> Word32 -> Memory -> IO ()
set n value reg = writeArray reg (n `mod` memorySize) value

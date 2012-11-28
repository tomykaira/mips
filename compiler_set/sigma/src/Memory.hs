module Memory where

import Data.Array.Unboxed
import Data.Word

type Memory = UArray Int Word32

memorySize = 0x200000

createMemory :: Memory
createMemory = array (0, memorySize-1) []

get :: Int -> Memory -> Word32
get n reg = reg ! (n `mod` memorySize)

set :: Int -> Word32 -> Memory -> Memory
set n value reg = reg // [(n `mod` memorySize, value)]

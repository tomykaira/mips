module Memory where

import Data.Array.Unboxed
import Data.Word

type Memory = UArray Int Word32

createMemory :: Memory
createMemory = array (1, 65535) []

get :: Int -> Memory -> Word32
get n reg = reg ! n

set :: Int -> Word32 -> Memory -> Memory
set n value reg = reg // [(n, value)]

module Register where

import Data.Array.Unboxed
import Data.Word

type RegisterFile = UArray Int Word32

createRegister :: RegisterFile
createRegister = array (1, 31) []

{-| function 'get'
  
>>> get 1 createRegister
0

>>> get 0 createRegister
0
 
-}

get :: Int -> RegisterFile -> Word32
get n reg = if n == 0 then 0 else reg ! n

{-| function 'set'
  
>>> get 2 (set 2 5 createRegister)
5

>>> get 1 (set 2 5 createRegister)
0

>>> get 0 (set 0 5 createRegister)
0
 
-}

set :: Int -> Word32 -> RegisterFile -> RegisterFile
set n value reg = reg // [(n, value)]

module Util where

import qualified Numeric
import Data.Bits
import Data.Int
import Data.Word

showHex :: (Integral a, Show a) => a -> String
showHex x = Numeric.showHex x ""

signExtend :: Int -> Word32 -> Word32
signExtend size value =
    if value .&. (1 `shiftL` (size-1)) > 0 then
        (((1 `shiftL` (32 - size)) - 1) `shiftL` size) .|. value
    else
        value

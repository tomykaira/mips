{-# LANGUAGE ForeignFunctionInterface #-}
{-# CFILES fpu.c #-}
module FPU where
    
import Data.Bits
import Data.Word

foreign import ccall "myfadd" fadd :: Word32 -> Word32 -> Word32
foreign import ccall "myfmul" fmul :: Word32 -> Word32 -> Word32
foreign import ccall "myfinv" finv :: Word32 -> Word32
foreign import ccall "myfsqrt" fsqrt :: Word32 -> Word32

fneg :: Word32 -> Word32
fneg x = x `xor` 0x80000000

compare :: Word32 -> Word32 -> Ordering
compare a b =
    case (sign a, sign b, compareNoSign) of
      (False, False, x) -> x
      (True, True, x) -> invert x
      (False, True, _) -> GT
      (True, False, _) -> LT
    where
      invert LT = GT
      invert EQ = EQ
      invert GT = LT
      sign x =
          (x `shiftR` 31) /= 0
      compareNoSign =
          (a .&. 0x7fffffff) `Prelude.compare` (b .&. 0x7fffffff)
      

module Binary where

import Data.List
import Data.Bits
import Data.Word
    
newtype Binary = Binary Integer deriving (Eq, Show)

instance Num Binary where
    fromInteger n = Binary . roll . map (read.return) . show $ n
        where
          roll = foldl' unstep 0

          unstep a 1 = a `shiftL` 1 .|. fromIntegral 1
          unstep a 0 = a `shiftL` 1
          unstep _ _ = error "Invalid character in binary literal"

toInteger (Binary i) = i
toInt (Binary i) = fromInteger i :: Int
toWord32 (Binary i) = fromInteger i :: Word32

type Opcode = Binary

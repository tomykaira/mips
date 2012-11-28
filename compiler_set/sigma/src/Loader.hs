-- Load program file and parse into a list of instructions
module Loader where

import qualified Numeric
import Data.Bits
import qualified Data.Either as Either
import qualified Data.List as List

import qualified Binary as B 
import MachineState
import Instruction

{-| function 'decodeFile'

>>> import Control.Monad.State
>>> let input = "0c030000\taddi $r3, $r0, 0\n0c040960\taddi $r4, $r0, 2400\n0c050041\taddi $r5, $r0, 65\n0c630001\taddi $r3, $r3, 1"
>>> let Right program = decodeFile input
>>> length program
4
>>> evalState (foldr (>>) (getI 4) program) (initialState program)
2400
>>> evalState (foldr (>>) (getI 3) program) (initialState program)
1

-}

decodeFile :: String -> Either [String] Program
decodeFile fileContent =
    let (errors, program) = Either.partitionEithers $ map decodeLine (lines fileContent) in
    if null errors then
        Right program
    else
        Left errors
    where
      decodeLine line =
          case words line of
            [] -> Left "Empty line found."
            code : _ ->
                case Numeric.readHex code of
                  (h, _) : _ ->
                      decode (fromInteger (h :: Integer))
                  _ -> Left $ "Code is not hexadecimal: " ++ line

{-| function 'decode'

addi $r4, $r0, 2400
>>> import Control.Monad.State
>>> let Right trans = decode 0x0c040960
>>> evalState (trans >> (getI 4)) (initialState [trans])
2400

-}

{-| function 'decode'

halt
>>> import Control.Monad.State
>>> let Right trans = decode 0xfc000000
>>> evalState trans (initialState [trans])
Halt

-}

decode :: ByteCode -> Either String StateTransformer
decode byteCode =
    case findMnemonic op of
      Just (m) -> Right $ createInstructionTransformer m byteCode
      Nothing -> Left $ "Could not decode byte code 0x" ++ Numeric.showHex byteCode ""
    where
      op :: B.Binary
      op = B.Binary . fromIntegral $ (byteCode `shiftR` 26) .&. 0x3f

      findMnemonic code =
          fmap mnemonic $ List.find (\inst -> opcode inst == code) operations

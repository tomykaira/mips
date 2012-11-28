module Instruction where

import Data.Bits
import Data.Word

import qualified Binary as B 
import qualified FPU
import MachineState

type ByteCode = Word32
type Opcode = Word32

data Instruction = Instruction {
      opcode    :: B.Binary
    , mnemonic  :: Mnemonic
    , operands  :: [Operand]
    , writeBack :: Maybe Operand}

data Mnemonic =
    ADD | SUB | XOR
  | ADDI | SUBI | XORI | SLLI | SRAI
  | FMVLO | FMVHI
  | IMOVF | FMOVI
  | FADD | FSUB | FMUL | FMULN | FINV | FSQRT
  | LDI | LDR | STI | FLDI | FSTI | FLDR
  | BEQ | BLT | BLE | FBEQ | FBLT | FBLE
  | J | JR | CALL | CALLR | RETURN
  | INPUTB | OUTPUTB
  | HALT
  | DEBUG
  | DISPLAY | READKEY

data Operand =
    RS | RT | RD | FRS | FRT | FRD | IMM | JumpIMM | RX | KEY

operations :: [Instruction]
operations = [
 Instruction { opcode = 000000
             , mnemonic = ADD
             , operands = [RS, RT]
             , writeBack = Just RD },
 Instruction { opcode = 000001
             , mnemonic = SUB
             , operands = [RS, RT]
             , writeBack = Just RD }, -- , execution = (\ -> setRD (rs - rt))
 Instruction {opcode = 000010
             , mnemonic = XOR
             , operands = [RS, RT]
             , writeBack = Just RD }, -- , execution = (\ -> setRD (rs `xor` rt))
 Instruction {opcode = 000011
             , mnemonic = ADDI
             , operands = [RS, IMM]
             , writeBack = Just RT }, -- , execution = (\ -> setRT (rs + imm))
 Instruction {opcode = 000100
             , mnemonic = SUBI
             , operands = [RS, IMM]
             , writeBack = Just RT }, -- , execution = (\ -> setRT (rs - imm))
 Instruction {opcode = 000101
             , mnemonic = XORI
             , operands = [RS, IMM]
             , writeBack = Just RT }, -- , execution = (\ -> setRT (rs `xor` imm))
 Instruction {opcode = 000110
             , mnemonic = SLLI
             , operands = [RS, IMM]
             , writeBack = Just RT }, -- , execution = (\ -> setRT (rs `shiftL` imm))
 Instruction {opcode = 000111
             , mnemonic = SRAI
             , operands = [RS, IMM]
             , writeBack = Just RT }, -- , execution = (\ -> setRT (rs `shiftR` imm))

 Instruction {opcode = 010010
             , mnemonic = FMVLO
             , operands = [FRS, IMM]
             , writeBack = Just FRT }, -- , execution = (\ -> setFRT (frs .|. (imm .&. 0xffff)))
 Instruction {opcode = 010011
             , mnemonic = FMVHI
             , operands = [IMM]
             , writeBack = Just FRT }, -- , execution = (\ -> setFRT ((imm .&. 0xffff) `shiftL` 16))
 Instruction {opcode = 010110
             , mnemonic = IMOVF
             , operands = [RS]
             , writeBack = Just FRT }, -- , execution = (\ -> setFRT rs)
 Instruction {opcode = 010111
             , mnemonic = FMOVI
             , operands = [FRS]
             , writeBack = Just RT }, -- , execution = (\ -> setRT frs)
 Instruction {opcode = 110000
              , mnemonic = FADD
              , operands = [FRS, FRT]
              , writeBack = Just FRD }, -- , execution = (\ -> setFRD (FPU.fadd FRS FRT))
 Instruction {opcode = 110001
             , mnemonic = FSUB
             , operands = [FRS, FRT]
             , writeBack = Just FRD }, -- , execution = (\ -> setFRD (FPU.fsub FRS FRT))
 Instruction {opcode = 110010
             , mnemonic = FMUL
             , operands = [FRS, FRT]
             , writeBack = Just FRD }, -- , execution = (\ -> setFRD (FPU.fmul FRS FRT))
 Instruction {opcode = 110011
             , mnemonic = FMULN
             , operands = [FRS, FRT]
             , writeBack = Just FRD }, -- , execution = (\ -> setFRD (FPU.fmuln FRS FRT))
 Instruction {opcode = 110100
             , mnemonic = FINV
             , operands = [FRS]
             , writeBack = Just FRD }, -- , execution = (\ -> setFRD (FPU.finv FRS))
 Instruction {opcode = 110101
             , mnemonic = FSQRT
             , operands = [FRS]
             , writeBack = Just FRD }, -- , execution = (\ -> setFRD (FPU.fsqrt FRS))
 Instruction {opcode = 101000
              , mnemonic = LDI
              , operands = [RS, IMM]
              , writeBack = Just RT }, -- , execution = (\ -> setRT (mem (rs + imm)))
 Instruction {opcode = 101001
             , mnemonic = STI
             , operands = [RS, RT, IMM]
             , writeBack = Nothing }, -- , execution = (\ -> setMem (rs + imm) rt)
 Instruction {opcode = 101100
             , mnemonic = LDR
             , operands = [RS, RT]
             , writeBack = Just RD }, -- , execution = (\ -> setRD (mem (rs + rt)))
 Instruction {opcode = 101010
             , mnemonic = FLDI
             , operands = [RS, IMM]
             , writeBack = Just FRT }, -- , execution = (\ -> setFRT (mem (rs + imm)))
 Instruction {opcode = 101011
             , mnemonic = FSTI
             , operands = [RS, FRT, IMM]
             , writeBack = Nothing }, -- , execution = (\ -> setMem (rs + imm) frt)
 Instruction {opcode = 101110
             , mnemonic = FLDR
             , operands = [RS, RT]
             , writeBack = Just FRD }, -- , execution = (\ -> setFRD (mem (rs + rt)))
 Instruction {opcode = 100000
              , mnemonic = BEQ
              , operands = [RS, RT, IMM]
              , writeBack = Nothing }, -- , execution = (\ -> if rs == rt then goto imm else ())
 Instruction {opcode = 100001
             , mnemonic = BLT
             , operands = [RS, RT, IMM]
             , writeBack = Nothing }, -- , execution = (\ -> if rs < rt then goto imm else ())
 Instruction {opcode = 100010
             , mnemonic = BLE
             , operands = [RS, RT, IMM]
             , writeBack = Nothing }, -- , execution = (\ -> if rs <= rt then goto imm else ())
 Instruction {opcode = 100100
             , mnemonic = FBEQ
             , operands = [FRS, FRT, IMM]
             , writeBack = Nothing }, -- , execution = (\ -> if frs == frt then goto imm else ())
 Instruction {opcode = 100101
             , mnemonic = FBLT
             , operands = [FRS, FRT, IMM]
             , writeBack = Nothing }, -- , execution = (\ -> if (FPU.lt frs frt) then goto imm else ())
 Instruction {opcode = 100110
             , mnemonic = FBLE
             , operands = [FRS, FRT, IMM]
             , writeBack = Nothing }, -- , execution = (\ -> if (FPU.le frs frt) then goto imm else ())

 Instruction {opcode = 111000
             , mnemonic = J
             , operands = [JumpIMM]
             , writeBack = Nothing }, -- , execution = (\ -> goto jumpImm)
 Instruction {opcode = 111001
             , mnemonic = JR
             , operands = [RS]
             , writeBack = Nothing }, -- , execution = (\ -> goto rs)
 Instruction {opcode = 111010
             , mnemonic = CALL
             , operands = [JumpIMM]
             , writeBack = Nothing }, -- , execution = (\ -> call jumpImm)
 Instruction {opcode = 111011
             , mnemonic = CALLR
             , operands = [RS]
             , writeBack = Nothing }, -- , execution = (\ -> call rs)
 Instruction {opcode = 111100
             , mnemonic = RETURN
             , operands = []
             , writeBack = Nothing }, -- , execution = (\ -> return)
 Instruction {opcode = 111101
             , mnemonic = INPUTB
             , operands = [RX]
             , writeBack = Just RT }, -- , execution = (\ -> setRT readRX)
 Instruction {opcode = 111110
             , mnemonic = OUTPUTB
             , operands = [RT]
             , writeBack = Nothing }, -- , execution = (\ -> sendTX rt)
 Instruction {opcode = 111111
             , mnemonic = HALT
             , operands = [RT]
             , writeBack = Nothing }, -- , execution = (\ -> halt)
 Instruction {opcode = 101111
             , mnemonic = DEBUG
             , operands = []
             , writeBack = Nothing }, -- , execution = (\ -> setRT readKey)

 Instruction {opcode = 001000
             , mnemonic = DISPLAY
             , operands = [RS, RT]
             , writeBack = Nothing }, -- , execution = (\ -> display rs rt)
 Instruction {opcode = 001001
             , mnemonic = READKEY
             , operands = []
             , writeBack = Just RT } -- , execution = (\ -> setRT readKey)
             ]

createInstructionTransformer :: Mnemonic -> ByteCode -> StateTransformer
createInstructionTransformer m inst =
    case m of
      ADD ->
          do a <- getI rs
             b <- getI rt
             setI rd (a + b)
      SUB ->
          do a <- getI rs
             b <- getI rt
             setI rd (a - b)
      XOR ->
          do a <- getI rs
             b <- getI rt
             setI rd (a `xor` b)
      ADDI ->
          do a <- getI rs
             setI rt (a + imm)
      SUBI ->
          do a <- getI rs
             setI rt (a - imm)
      XORI ->
          do a <- getI rs
             setI rt (a `xor` imm)
      SLLI ->
          do a <- getI rs
             setI rt (a `shiftL` (fromIntegral imm))
      SRAI ->
          do a <- getI rs
             setI rt (a `shiftR` (fromIntegral imm))

      FMVLO ->
          do a <- getF rs
             setF rt (a .|. (imm .&. 0xffff))
      FMVHI ->
          setF rt ((imm .&. 0xffff) `shiftL` 16)
      IMOVF ->
          getI rs >>= setF rt
      FMOVI ->
          getF rs >>= setI rt

      FADD ->
          do a <- getF rs
             b <- getF rt
             setF rd (FPU.fadd a b)
      FSUB ->
          do a <- getF rs
             b <- getF rt
             setF rd (FPU.fadd a (FPU.fneg b))
      FMUL ->
          do a <- getF rs
             b <- getF rt
             setF rd (FPU.fmul a b)
      FMULN ->
          do a <- getF rs
             b <- getF rt
             setF rd (FPU.fmul a (FPU.fneg b))
      FINV ->
          do a <- getF rs
             setF rd (FPU.finv a)
      FSQRT ->
          do a <- getF rs
             setF rd (FPU.fsqrt a)

      LDI ->
          do base <- getI rs
             a <- mem (base + imm)
             setI rt a
      STI ->
          do base <- getI rs
             a <- getI rt
             setMem (base + imm) a
      LDR ->
          do base <- getI rs
             a <- mem (base + imm)
             setI rt a
      FLDI ->
          do base <- getI rs
             a <- mem (base + imm)
             setF rt a
      FSTI ->
          do base <- getI rs
             a <- getF rt
             setMem (base + imm) a
      FLDR ->
          do base <- getI rs
             a <- mem (base + imm)
             setF rt a

      BEQ ->
          do a <- getI rs
             b <- getI rt
             if a == b then gotoRelative imm else next
      BLT ->
          do a <- getI rs
             b <- getI rt
             if a < b  then gotoRelative imm else next
      BLE ->
          do a <- getI rs
             b <- getI rt
             if a <= b then gotoRelative imm else next
      FBEQ ->
          do a <- getF rs
             b <- getF rt
             if a == b then gotoRelative imm else next
      FBLT ->
          do a <- getF rs
             b <- getF rt
             if FPU.compare a b == LT then gotoRelative imm else next
      FBLE ->
          do a <- getF rs
             b <- getF rt
             if FPU.compare a b == GT then next else gotoRelative imm

      J ->
          goto jumpImm
      JR ->
          getI rs >>= goto
      CALL ->
          call jumpImm
      CALLR ->
          getI rs >>= call
      RETURN ->
          ret
      INPUTB ->
          do a <- readRx
             setI rt a
      OUTPUTB ->
          do a <- getI rt
             sendTx a
      HALT -> halt

      -- TODO:
      DEBUG -> next
      where
        rs = fromIntegral (inst `shiftR` 21) .&. 0x1f

        rt = fromIntegral (inst `shiftR` 16) .&. 0x1f

        rd = fromIntegral (inst `shiftR` 11) .&. 0x1f

        imm =
            if inst .&. 0x8000 > 0 then
                (0xffff `shiftL` 16) .|. (inst .&. 0xffff)
            else
                inst .&. 0xffff

        jumpImm = inst .&. 0x3ffffff

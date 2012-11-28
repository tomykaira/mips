module MachineState where

import qualified Control.Monad.State as State
import Data.Word
import qualified Data.ByteString as B

import Debug.Trace

import qualified Register as Reg
import qualified Memory as Mem
import qualified BoundedStore as Store
import Util

data MachineState = MachineState {
      intRegister    :: Reg.RegisterFile
    , floatRegister  :: Reg.RegisterFile
    , memory         :: Mem.Memory
    , program        :: Program
    , programCounter :: ProgramCounter
    , callStack      :: [ProgramCounter]
    , rxInput        :: B.ByteString
    , txOutput       :: B.ByteString
    , recentStates   :: Store.BoundedStore MachineState
    , operationLog   :: [Incident]}

instance Show MachineState where
    show m =
        "PC: " ++ show (programCounter m) ++ "\n" ++
        "IntRegisters: " ++ show (intRegister m)

data ExecutionState = Continue | Halt String deriving (Show)

type StateTransformer = State.State MachineState ExecutionState
type Program = [StateTransformer]
type ProgramCounter = Word32

data Incident = DispatchInstruction (ProgramCounter)
              | WriteIntRegister (ProgramCounter, Int, Word32)
              | WriteFloatRegister (ProgramCounter, Int, Word32)
              | SendOutput (ProgramCounter, Word8)
              | WriteMemory (ProgramCounter, Word32, Word32)

instance Show Incident where
    show (DispatchInstruction (pc)) = 
        "INST " ++ show pc
    show (WriteIntRegister (pc, address, value)) =
        "REG:  " ++ show address ++ " " ++ showHex value
    show (WriteFloatRegister (pc, address, value)) =
        "REG:  " ++ show address ++ " " ++ showHex value
    show (SendOutput (pc, value)) =
        "IO " ++ show pc ++ " " ++ showHex value
    show (WriteMemory (pc, address, value)) =
        "MEM " ++ show pc ++ " " ++ showHex address ++ " " ++ showHex value

initialState :: Program -> MachineState
initialState prog =
    MachineState { intRegister = Reg.createRegister
                 , floatRegister = Reg.createRegister
                 , memory = Mem.createMemory
                 , program = prog
                 , programCounter = 0
                 , callStack = []
                 , rxInput = B.pack [0, 0, 0, 10]
                 , txOutput = B.empty
                 , recentStates = Store.create 10
                 , operationLog = [] }

put :: MachineState -> State.State MachineState ()
put = State.put
    -- do
    --   current <- State.get
    --   State.put current { recentStates = Store.add current (recentStates current)}

{-| function 'getI'
  
>>> evalState (getI 1) (initialState [])
0

-}

getI :: Int -> State.State MachineState Word32
getI n = State.get >>= (return . (Reg.get n) . intRegister)

{-| function 'setI'
  
>>> evalState (setI 1 6) (initialState [])
Continue

>>> evalState (setI 1 1025 >> getI 1) (initialState [])
1025

-}

setI :: Int -> Word32 -> State.State MachineState ExecutionState
setI n value =
    if n == 0 then
        next
    else
        do
          st <- State.get
          put st { intRegister = (Reg.set n value) $ intRegister st
                 , operationLog = WriteIntRegister (programCounter st, n, value) : operationLog st}
          next

getF :: Int -> State.State MachineState Word32
getF n = State.get >>= (return . (Reg.get n) . floatRegister)

{-| function 'setF'
  
>>> evalState (setF 1 6) (initialState [])
Continue

>>> evalState (setF 1 1025 >> getF 1) (initialState [])
1025

-}

setF :: Int -> Word32 -> State.State MachineState ExecutionState
setF n value =
    do st <- State.get
       put st { floatRegister = (Reg.set n value) $ floatRegister st
              , operationLog = WriteFloatRegister (programCounter st, n, value) : operationLog st }
       next

{-| function 'goto'
  
>>> let (value, state) = runState (goto 1292) (initialState [])
>>> value
Continue
>>> programCounter state
1292

-}

goto :: Word32 -> State.State MachineState ExecutionState
goto pc =
    do
      st <- State.get
      put st { programCounter = pc }
      return Continue

next :: State.State MachineState ExecutionState
next =
    do
      st <- State.get
      put st { programCounter = programCounter st + 1 }
      return Continue

gotoRelative :: Word32 -> State.State MachineState ExecutionState
gotoRelative offset =
    do
      st <- State.get
      let pc = programCounter st
      goto (pc + offset)

{-| function 'halt'
  
>>> evalState halt (initialState [])
Halt

-}

halt :: State.State MachineState ExecutionState
halt = return $ Halt "Normal exit"

{-| function 'readRx'
  
>>> evalState (readRx >> readRx >> readRx >> readRx) (initialState [])
10
  
-}

readRx :: State.State MachineState Word32
readRx =
    do
      st <- State.get
      case B.uncons (rxInput st) of
        Just(hd, tl) ->
            do
              put st {rxInput = tl}
              return (fromIntegral hd)
        Nothing ->
            -- FIXME: What can I do? Handle error
            return 0

{-| function 'sendTx'
  
>>> (B.head . txOutput . execState (sendTx 5)) (initialState [])
5
  
-}

sendTx :: Word32 -> State.State MachineState ExecutionState
sendTx value =
    do
      st <- State.get
      let xff = fromIntegral value
      put st { txOutput = B.cons xff (txOutput st)
             , operationLog = SendOutput (programCounter st, xff) : operationLog st}
      next

{-| function 'mem'
  
>>> evalState (setMem 3 9292 >> mem 3) (initialState [])
9292

-}

mem :: Word32 -> State.State MachineState Word32
mem address =
    State.get >>= (return . Mem.get (fromIntegral address) . memory)

setMem :: Word32 -> Word32 -> State.State MachineState ExecutionState
setMem address value =
    do
      st <- State.get
      put st { memory = Mem.set (fromIntegral address) value (memory st)
             , operationLog = WriteMemory (programCounter st, address, value) : operationLog st}
      next

{-| function 'call'
  
>>> programCounter $ evalState (call 929 >> return) ((initialState []) {programCounter = 224})
224
  
 -}

call :: Word32 -> State.State MachineState ExecutionState
call address =
    do
      st <- State.get
      put st { callStack = (programCounter st) : (callStack st) }
      goto address

ret :: State.State MachineState ExecutionState
ret =
    do
      st <- State.get
      -- TODO: error report
      let top : rest = callStack st
      put st { callStack = rest }
      goto (top + 1)

fetchInstruction :: State.State MachineState ExecutionState
fetchInstruction =
    do st <- State.get
       put st { operationLog = (DispatchInstruction (programCounter st)) : operationLog st }
       let nextPC = (fromIntegral $ programCounter st)
       if nextPC < length (program st) then program st !! nextPC else return $ Halt ("Illegal PC: " ++ show nextPC)

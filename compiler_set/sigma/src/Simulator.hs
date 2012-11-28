-- do simulation
module Simulator where

import Control.Monad.State
import qualified Data.ByteString as B

import MachineState hiding (program)
import qualified BoundedStore as Store

execute :: Program -> IO ()
execute program =
    do
      let (exitReason, lastState) = runState evalLoop (initialState program)
      putStrLn $ "Simulation done. " ++ exitReason
      putStrLn "Output bytes"
      print $ (B.reverse . txOutput) lastState
      -- print (Store.toList (recentStates lastState))

{-| function 'execute'

add     $r0, $r0, $r0
addi $r3, $r0, 5
addi $r4, $r0, 5
beq $r3, $r4, taken
addi $r3, $r0, 0
halt
addi $r5, $r0, 9
halt

>>> import Control.Monad.State
>>> let input = "00000000\n0c030005\n0c040005\n80640003\n0c030000\nfc000000\n0c050009\nfc000000"
>>> let Right program = Loader.decodeFile input
>>> length program
8
>>> let state = execState evalLoop (initialState program)
>>> evalState (getI 3) state
5
>>> evalState (getI 4) state
5
>>> evalState (getI 5) state
9

-}

evalLoop :: State MachineState String
evalLoop =
    fetchInstruction >>=
                         (\jump ->
                              case jump of
                                Continue -> evalLoop
                                Halt reason -> return reason)


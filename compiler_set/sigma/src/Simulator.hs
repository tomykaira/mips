-- do simulation
module Simulator where

import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.Maybe as Maybe
import Data.Char (chr)

import MachineState hiding (program)
import qualified BoundedStore as Store

data Options =
    Options { optLogging  :: Bool
            , optStdout   :: Bool
            , optTestMode :: Bool
            , optInput    :: Maybe String }

startOptions :: Options
startOptions = Options {
                 optLogging  = False
               , optStdout   = True
               , optTestMode = False
               , optInput    = Nothing }

execute :: Options -> Program -> IO ()
execute options program =
    do
      inputContent <- Maybe.maybe (return B.empty) B.readFile (optInput options)
      init <- initialState program (optLogging options) inputContent
      runStateT evalLoop init >>= report

    where
      report (exitReason, lastState) = 
          if optTestMode options then
              putStr $ ((map (chr. fromIntegral)) . B.unpack . B.reverse . stripExitCode . txOutput) lastState
          else
              do
                mapM_ print (Maybe.maybe [] reverse (operationLog lastState))
                putStrLn $ "Simulation done. " ++ exitReason
                putStrLn "Output bytes"
                print $ (B.reverse . txOutput) lastState

      stripExitCode output =
          let (code, rest) = B.splitAt 3 output in
          if code == B.pack [130, 181, 231] then
              rest
          else
              output

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

evalLoop :: MachineState String
evalLoop =
    fetchInstruction >>=
                         (\jump ->
                              case jump of
                                Continue -> evalLoop
                                Halt reason -> return reason)


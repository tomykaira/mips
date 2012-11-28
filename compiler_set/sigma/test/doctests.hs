module Main where

import Test.DocTest
import qualified FPU
import System.Environment (getEnv)

main :: IO ()
main =
    do
      fpuDirectory <- getEnv "FPUDATADIR"
      FPU.loadTables fpuDirectory
      doctest ["--optghc=-Lcabal-dev/lib",
                "--optghc=-package-conf=cabal-dev/packages-7.4.1.conf",
                "src/FPU.hs", "src/Binary.hs", "src/Instruction.hs",
                "src/MachineState.hs", "src/Memory.hs", "src/Register.hs",
                "src/Simulator.hs", "src/Loader.hs"]

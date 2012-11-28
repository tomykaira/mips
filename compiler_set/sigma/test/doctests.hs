module Main where

import Test.DocTest

main :: IO ()
main = doctest ["--optghc=-Lcabal-dev/lib",
                "--optghc=-package-conf=cabal-dev/packages-7.4.1.conf",
                "src/FPU.hs", "src/Binary.hs", "src/Instruction.hs", "src/Loader.hs",
                "src/MachineState.hs", "src/Memory.hs", "src/Register.hs",
                "src/Simulator.hs"]

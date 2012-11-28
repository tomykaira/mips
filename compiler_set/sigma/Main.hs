module Main where

import System.Environment (getArgs, getEnv)

import qualified FPU (loadTables)
import qualified Loader
import qualified Simulator

main :: IO ()
main =
    do
      (binFile : _) <- getArgs
      fpuDirectory <- getEnv "FPUDATADIR"
      FPU.loadTables fpuDirectory
      binaryContent <- readFile binFile
      case Loader.decodeFile binaryContent of
        Left errors ->
            mapM_ putStrLn errors
        Right program ->
            Simulator.execute program

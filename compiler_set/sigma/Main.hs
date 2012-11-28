module Main where

import System.Environment (getArgs)

import qualified Loader
import qualified Simulator

main :: IO ()
main =
    do
      (binFile : _) <- getArgs
      binaryContent <- readFile binFile
      case Loader.decodeFile binaryContent of
        Left errors ->
            mapM_ putStrLn errors
        Right program ->
            Simulator.execute program

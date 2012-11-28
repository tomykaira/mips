module Main (main) where

import qualified Data.Text as T
import System.Console.GetOpt
import System.Environment (getArgs, getEnv)
import System.Exit (exitSuccess, exitFailure)

import qualified FPU (loadTables)
import qualified Loader
import Simulator

main :: IO ()
main =
    do
      args <- getArgs

      -- Parse options, getting a list of option actions
      let (actions, nonOptions, parseErrors) = getOpt RequireOrder options args

      if not (null parseErrors) then mapM_ putStrLn parseErrors >> exitFailure else return ()
 
      -- Here we thread startOptions through all supplied option actions
      let opts = foldl (.) id actions $ startOptions
 
      let (binFile : _) = nonOptions
      fpuDirectory <- getEnv "FPUDATADIR"
      FPU.loadTables fpuDirectory
      binaryContent <- readFile binFile

      case Loader.decodeFile binaryContent of
        Left errors ->
            mapM_ putStrLn errors
        Right program ->
            Simulator.execute opts program

options :: [ OptDescr (Options -> Options) ]
options =
    [ Option "f" ["input"]
      (ReqArg
       (\arg opt -> opt { optInput = Just arg })
       "FILE")
      "Input file"
 
    , Option "l" ["logging"]
      (ReqArg
       (\arg opt -> opt { optLogging = map T.unpack (T.splitOn (T.pack ",") (T.pack arg)) })
       "TARGETS")
      "Logging targets"
 
    , Option "t" ["test"]
      (NoArg
       (\opt -> opt { optTestMode = True }))
      "Enable test mode (no output other than from OUTPUTB)"
 
    , Option "S" ["no-stdout"]
      (NoArg
       (\opt -> opt { optStdout = False }))
      "Disable STDOUT"
    ]              

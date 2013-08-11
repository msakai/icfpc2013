module Main where

import Data.Char (toLower)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Environment
import System.Exit
import System.IO

import BV
import Utility

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering

  args <- getArgs
  case args of
    [pid, ops', size'] -> do
      let ops  = words ops'
          size = read size'
      if Set.fromList ops `Set.isSubsetOf`  allOps
      then do
        putStrLn "============================="
        putStrLn $ "pid = " ++ pid
        putStrLn $ "ops = " ++ show ops
        putStrLn $ "size = " ++ show size
        putStrLn "============================="
        guessMania pid ops size
      else do
        putStrLn $ "unknown operators: " ++ show (Set.toList (Set.fromList ops `Set.difference` allOps))
        exitFailure
    _ -> do
      putStrLn "usage: GuessMania pid \"op1 op2 op3 .. \" size"
      return ()

allOps :: Set String
allOps =
  Set.fromList $        
    ["if0","fold","tfold","bonus"] ++
    [render o | o <- [(minBound::Op1) .. maxBound]] ++
    [render o | o <- [(minBound::Op2) .. maxBound]]


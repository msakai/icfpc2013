module Main where

import Control.Monad (forM_)
import Data.Char (toLower)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Environment
import System.Exit
import System.IO

import BV
import Interaction
import Utility

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering

  args <- getArgs
  case args of
    ["train", size, n] -> do
      let (size', n') = (read size, read n)
      forM_ [1..n'] $ \n -> trainTest [] size'
    ["real", size] -> do
      Just probs <- myproblems
      let (probs', size') = (filter (\p -> probSize p == size') probs, read size)
      forM_ probs' realTest
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
      putStrLn "       GuessMania train size cycle"
      putStrLn "       GuessMania real size"
      return ()

allOps :: Set String
allOps =
  Set.fromList $        
    ["if0","fold","tfold","bonus"] ++
    [render o | o <- [(minBound::Op1) .. maxBound]] ++
    [render o | o <- [(minBound::Op2) .. maxBound]]


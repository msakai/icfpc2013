module Utility 
       ( realTest
       , trainTest
       , guessMania
         -- data
       , myproblems
       ) where

import BV
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Data.Aeson (decode)
import Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe (fromJust, isJust)
import GenProgram.DP
import Interaction
import System.Exit
import System.IO (hFlush, stdout)


-- myproblems
myproblems :: IO (Maybe [Problem])
myproblems = fmap (decode . BL.pack) $ readFile "data/myproblems.json"

-- | High level test utility on Training mode
-- 
trainTest :: [String] -> Int -> IO ()
trainTest ops n = do
  p <- training (Just n) (Just ops)
  putStrLn $ "TRAINING PROBLEM :: " ++ show p
  case fst p of
    (2,0,0) -> if isJust (snd p)
               then do
                 let Success tp = fromJust (snd p)
                 guessMania <$> trprId <*> trprOperators <*> trprSize $ tp
               else exitFailure
    x -> putStrLn $ show x

-- | High level test utility Real Problem mode
realTest :: Problem -> IO ()
realTest = guessMania <$> probId <*> probOperators <*> probSize

-- | robust guess engine
--
guessMania :: ProbId -> [String] -> Int -> IO ()
guessMania pid ops n = do 
  let (ps, l) = (generate ops n, length ps)
  putStr $ "We have " ++ show l ++ " programs, Are you continue? (yes|no)> "
  hFlush stdout
  yn <- return "yes" -- getLine
  case yn of
    "yes" -> do
      ps' <- evalMania Nothing pid ps
      putStrLn $ "Targetting " ++ show (length ps') ++ " programs..."
      go ps'
    _ -> putStrLn "stopping..."
  where
    go :: ProgramSet -> IO ()
    go [] = putStrLn "[ERROR!] we can't find the function." >> return ()
    go (p:ps) = do
      r <- submitGuess pid (render p)
      case fst r of
        (2,0,0) -> if isJust (snd r)
                   then do
                     let Success gr = fromJust $ snd r
                     case gsrsStatus gr of
                       "win" -> putStrLn (render p ++ " => " ++ gsrsStatus gr)
                       "mismatch" -> do
                         putStrLn (render p ++ " => " ++ gsrsStatus gr)
                         putStrLn $ show $ gsrsValues gr
                         ps' <- evalMania (gsrsValues gr) pid ps
                         putStrLn $ "Targetting " ++ show (length ps') ++ " programs..."
                         go ps'
                       _ -> putStrLn (render p ++ " => " ++ gsrsStatus gr) >> go ps -- error occured
                   else putStrLn "[ERROR!] response body nothing."
        (4,1,2) -> putStrLn "solved!"
        (4,1,0) -> putStrLn "gone!"
        -- 429 : 1秒waitにしているけど根拠は単にtrainTestを連発してみて問題なさげだったからです.
        (4,2,9) -> putStrLn (render p ++ " sleep 1sec and try again ") >> hFlush stdout >> threadDelay (10^6) >> go (p:ps)
        x -> putStrLn (show x)

-- | robast eval engine
--
evalMania :: Maybe [Arg] -> ProbId -> ProgramSet -> IO ProgramSet
evalMania mTestCase pid progs = do
  let testCase = maybe initTestCase id mTestCase
  r <- evalProgram (Left pid) testCase
  case fst r of
    (2,0,0) -> do
      let Just (Success er) = snd r
          Just outs = evrsOutputs er
          inOut = zip (map read testCase) (map read outs)
      return $ filterByExamples progs inOut
    (4,1,2) -> putStrLn "solved!" >> return []
    (4,1,0) -> putStrLn "gone!" >> return []
    (4,2,9) -> evalMania mTestCase pid progs
    x -> putStrLn (show x) >> evalMania mTestCase pid progs
  where
    initTestCase = [ "0xFFFFFFFFFFFFFFFF"
                   , "0x0000000000000000"
                   ]

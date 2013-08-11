module GenProgram.Pruned (generate) where

import Control.Arrow
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.State
import Data.Aeson (decode)
import Data.Aeson.Types as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toLower)
import Data.List (find,delete)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Exit
import System.IO (hFlush, stdout)

import BV
import Interaction

{-
generate :: [String] -> Int -> [Program]
generate ops n = filter (flip isValidFor ops) $ evalStateT (genProgram ops) n

genProgram :: [String] -> Gen Program
genProgram ops = 
  if "tfold" `elem` ops
  then do
    let (x:y:vs) = allVars
    consumeSize 5 -- Program[1] + Fold[2] + (Var x)[1] + (Const Zero)[1]
    e <- genExpr ops [y,x] vs
    unused <- get
    guard $ unused == 0
    return $ Program x (Fold (Var x) (Const Zero) x y e)
  else do
    let (v:vs) = allVars
    consumeSize 1
    e <- genExpr ops [v] vs
    unused <- get
    guard $ unused == 0
    return $ Program v e

-- 状態は残りサイズ
type Gen = StateT Int []

ibind :: Gen a -> (a -> Gen a) -> Gen a
ibind g f = StateT $ \s ->
  interleaveN [runStateT (f a) s' | (a, s') <- runStateT g s]

isum :: [Gen a] -> Gen a
isum gs = StateT $ \s ->
  interleaveN [runStateT g s | g <- gs]

genExpr :: [String] -> [ID] -> [ID] -> Gen Expr
genExpr ops fvs unused =
  isum
  [ do consumeSize 1
       msum $ map return $ [Const b | b <- [Zero, One]]
  , do consumeSize 1
       msum $ map return $ [Var v | v <- fvs]
  , do consumeSize 1
       if "if0" `elem` ops
         then do e0 <- genExpr ops fvs unused
                 e1 <- genExpr ops fvs unused
                 e2 <- genExpr ops fvs unused
                 return $ If0 e0 e1 e2
         else mzero
  , do consumeSize 2
       if "fold" `elem` ops
          then do case unused of
                    (x:y:unused') -> do
                      -- TODO: ibindで対角的に列挙すべき?
                      e0 <- genExpr ops fvs unused
                      e1 <- genExpr ops fvs unused
                      e2 <- genExpr ops (y:x:fvs) unused'
                      return $ Fold e0 e1 x y e2
                    _ -> mzero
         else mzero
  , do consumeSize 1
       o <- msum $ map return $ toOps ops
       e <- genExpr ops fvs unused
       return $ Op1 o e
  , do consumeSize 1
       o <- msum $ map return $ toOps ops
       -- TODO: ibindで対角的に列挙すべき?
       e1 <- genExpr ops fvs unused
       e2 <- genExpr ops fvs unused
       return $ Op2 o e1 e2
  ]
  where
    toOps :: (Eq a, Enum a, Bounded a, Ord a, Show a) => [String] -> [a]
    toOps xs = [fromJust x | x <- map (flip lookup optbl) xs, isJust x]
      where
        optbl :: (Eq a, Enum a, Bounded a, Ord a, Show a) => [(String, a)]
        optbl = map (map toLower . show &&& id) [minBound..maxBound]

consumeSize :: Int -> Gen ()
consumeSize n = do
  unused <- get
  guard $ unused >= n
  put $! unused - n
  return ()
-}

-- improved version

generate :: [String] -> Int -> [Program]
generate ops n = filter (flip isValidFor ops) $ evalStateT (genProgram ops) (n,ops)

genProgram :: [String] -> Gen Program
genProgram ops = 
  if "tfold" `elem` ops
  then do
    let (x:y:vs) = allVars
    consumeSize 5 -- Program[1] + Fold[2] + (Var x)[1] + (Const Zero)[1]
    consumeOp "tfold"
    e <- genExpr (delete "tfold" ops) [y,x] vs
    (unused,ops) <- get
    guard $ unused == 0
    return $ Program x (Fold (Var x) (Const Zero) x y e)
  else do
    let (v:vs) = allVars
    consumeSize 1
    e <- genExpr ops [v] vs
    (unused,ops) <- get
    guard $ unused == 0
    return $ Program v e

-- 状態は(残りサイズ,残りオペレータ)
type Gen = StateT (Int,[String]) []

ibind' :: Gen a -> (a -> Gen a) -> Gen a
ibind' g f = StateT $ \s ->
  interleaveN [runStateT (f a) s' | (a, s') <- runStateT g s]

isum' :: [Gen a] -> Gen a
isum' gs = StateT $ \s ->
  interleaveN [runStateT g s | g <- gs]

genExpr :: [String] -> [ID] -> [ID] -> Gen Expr
genExpr ops fvs unused =
  isum'
  [ do consumeSize 1
       msum $ map return $ [Const b | b <- [Zero, One]]
  , do consumeSize 1
       msum $ map return $ [Var v | v <- fvs]
  , do consumeSize 1
       consumeOp "if0"
       if "if0" `elem` ops
         then do e0 <- genExpr ops fvs unused
                 e1 <- genExpr ops fvs unused
                 e2 <- genExpr ops fvs unused
                 return $ If0 e0 e1 e2
         else mzero
  , do consumeSize 2
       consumeOp "fold"
       if "fold" `elem` ops
          then do case unused of
                    (x:y:unused') -> do
                      -- TODO: ibindで対角的に列挙すべき?
                      e0 <- genExpr ops fvs unused
                      e1 <- genExpr ops fvs unused
                      e2 <- genExpr ops (y:x:fvs) unused'
                      return $ Fold e0 e1 x y e2
                    _ -> mzero
         else mzero
  , do consumeSize 1
       o <- msum $ map return $ toOps ops
       consumeOp (render o)
       e <- genExpr ops fvs unused
       return $ Op1 o e
  , do consumeSize 1
       o <- msum $ map return $ toOps ops
       consumeOp (render o)
       -- TODO: ibindで対角的に列挙すべき?
       e1 <- genExpr ops fvs unused
       e2 <- genExpr ops fvs unused
       return $ Op2 o e1 e2
  ]
  where
    toOps :: (Eq a, Enum a, Bounded a, Ord a, Show a) => [String] -> [a]
    toOps xs = [fromJust x | x <- map (flip lookup optbl) xs, isJust x]
      where
        optbl :: (Eq a, Enum a, Bounded a, Ord a, Show a) => [(String, a)]
        optbl = map (map toLower . show &&& id) [minBound..maxBound]

consumeSize :: Int -> Gen ()
consumeSize n = do
  (sz,ops) <- get
  let sz' = sz - n
  guard $ sz' >= 0
  put $! (sz',ops)
  return ()

consumeOp :: String -> Gen ()
consumeOp o = do 
  (sz,ops) <- get
  let ops' = delete o ops
  let needs = sum (map need (o:ops'))
  guard $ sz >= needs
  put $! (sz,ops')
  return ()

op1s,op2s :: [String]
op1s = ["not","shl1","shr1","shr4","shr16"]
op2s = ["and","or","xor","plus"]


need :: String -> Int
need o | elem o op1s  = 1
       | elem o op2s  = 2
       | o == "if0"   = 3
       | o == "fold"  = 4
       | otherwise    = 1

allVars :: [ID]
allVars = ["x" ++ show i | i <- [(1::Int)..]]

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave [] ys = ys

interleaveN :: [[a]] -> [a]
interleaveN [] = []
interleaveN ([]:xss) = interleaveN xss
interleaveN ((x:xs):xss) = x : interleaveN (xss ++ [xs])

-- myproblems
myproblems :: IO (Maybe [Problem])
myproblems = fmap (decode . BL.pack) $ readFile "data/myproblems.json"

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

realTest :: Problem -> IO ()
realTest = guessMania <$> probId <*> probOperators <*> probSize

guessMania :: ProbId -> [String] -> Int -> IO ()
guessMania pid ops n = do 
  let (ps, l) = (generate ops n, length ps)
  if l >= 5000 -- 適当に実験してきめる
    then do putStrLn $ "We have " ++ show l ++ " programs, which is exactly timeover on current tactics(bruteforce)"
            putStrLn "stopping..."
    else do putStr $ "We have " ++ show l ++ " programs, Are you continue? (yes|no)> "
            yn <- getLine
            case yn of
              "yes" -> do
                ps' <- evalMania pid ps
                putStrLn $ "Targetting " ++ show (length ps') ++ " programs..."
                go ps'
              _ -> putStrLn "stopping..."
  where
    go :: [Program] -> IO ()
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
                         let Just testCase = gsrsValues gr
                         r <- evalProgram (Left pid) testCase
                         case fst r of
                           (2,0,0) -> do
                             let Just (Success er) = snd r
                                 Just outs = evrsOutputs er
                                 inOut = zip (map read testCase) (map read outs)
                                 ps' = filter (match inOut) ps
                             putStrLn $ "Targetting " ++ show (length ps') ++ " programs..."
                             go ps'
                           x -> putStrLn $ show x
                       _ -> putStrLn (render p ++ " => " ++ gsrsStatus gr) >> go ps -- FIXME: こんなんある?
                   else putStrLn "[ERROR!] response body nothing."
        (4,1,2) -> putStrLn "solved!"
        (4,1,0) -> putStrLn "gone!"
        -- 429 : 1秒waitにしているけど根拠は単にtrainTestを連発してみて問題なさげだったからです.
        (4,2,9) -> putStrLn (render p ++ " sleep 1sec and try again ") >> hFlush stdout >> threadDelay (10^6) >> go (p:ps)
        x -> putStrLn (show x)


evalMania :: ProbId -> [Program] -> IO [Program]
evalMania pid progs = do
  r <- evalProgram (Left pid) testCase
  case fst r of
    (2,0,0) -> do
      let Just (Success er) = snd r
          Just outs = evrsOutputs er
          inOut = zip (map read testCase) (map read outs)
      return $ filter (match inOut) progs
    x -> undefined
  where
    testCase = [ "0xFFFFFFFFFFFFFFFF"
               , "0x0000000000000000"
               ]

match :: [(BV.Value, BV.Value)] -> Program -> Bool
match xs p = all (\(i, o) -> eval p i == o) xs

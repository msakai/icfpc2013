module GenProgram.DP
  (
    ProgramSet
  , generate
  , pickup
  , filterByExamples
  ) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Char (toLower)
import Data.List (find)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust, isJust, listToMaybe)
import qualified Data.Set as Set
import Data.Set (Set)

import BV

type ProgramSet = [Program]

pickup :: ProgramSet -> Maybe Program
pickup = listToMaybe

filterByExamples :: ProgramSet -> [(Value,Value)] -> ProgramSet
filterByExamples ps es = filter match ps
  where    
    match :: Program -> Bool
    match p = all (\(i, o) -> eval p i == o) es

generate :: [String] -> Int -> [Program]
generate ops n = filter (flip isValidFor ops) $ genProgram n ops

-- サイズとスコープ内の変数のリストをキーとしてメモ化
type M = State (Map (Int,[ID]) [Expr])

genProgram :: Int -> [String] -> [Program]
genProgram size ops = 
  if "tfold" `elem` ops
  then
    let (x:y:vs) = allVars
        es = evalState (genExpr (size - 5) ([y,x],vs) ops) Map.empty
    in [Program x (Fold (Var x) (Const Zero) x y e) | e <- es]
  else
    let (v:vs) = allVars
        es = evalState (genExpr (size - 1) ([v],vs) ops) Map.empty
    in [Program v e | e <- es]

genExpr :: Int -> ([ID],[ID]) -> [String] -> M [Expr]
genExpr size _ _ | size <= 0 = return []
genExpr size (fvs,unused) ops = do
  table <- get
  case Map.lookup (size,fvs) table of
    Just es -> return es
    Nothing -> do
      es <- liftM concat $ sequence $
        [ return [Const b | size == 1, b <- [Zero, One]]
        , return [Var v   | size == 1, v <- fvs]
        , liftM concat $ forM [(s0,s1) | "if0" `elem` ops, s0 <- [1..size-1], s1 <- [1..size-1-s0]] $ \(s0,s1) -> do
            let s2 = size-1-s0-s1
            es0 <- genExpr s0 (fvs,unused) ops
            es1 <- genExpr s1 (fvs,unused) ops
            es2 <- genExpr s2 (fvs,unused) ops
            let case0 = [If0 (Const Zero) e1 e2 | s0 == 1, e1 <- es1, e2 <- take 1 es2]
                case1 = [If0 (Const One)  e1 e2 | s0 == 1, e1 <- take 1 es1, e2 <- es2]
                case2 = [If0 e0 e1 e2 | e0 <- es0, e0 /= Const Zero, e0 /= Const One, e1 <- es1, e2 <- es2]
            return $ case0 ++ case1 ++ case2
        , liftM concat $ forM [(s0,s1) | "fold" `elem` ops, s0 <- [1..size-2], s1 <- [1..size-2-s0]] $ \(s0,s1) -> do
            let s2 = size-2-s0-s1
            es0 <- genExpr s0 (fvs,unused) ops
            es1 <- genExpr s1 (fvs,unused) ops
            let v1:v2:unused' = unused
                fvs' = v2 : v1 : fvs
            es2 <- genExpr s2 (fvs', unused') ops
            return [Fold e0 e1 v1 v2 e2 | e0 <- es0, e1 <- es1, e2 <- es2]
        , do es <- genExpr (size-1) (fvs,unused) ops
             return [Op1 o e | o <- toOps ops, e <- es]
        , liftM concat $ forM [1..size-1] $ \s0 -> do
            let s1 = size-1-s0
            es0 <- genExpr s0 (fvs,unused) ops
            es1 <- genExpr s1 (fvs,unused) ops
            let esElse = [Op2 o e0 e1 | o <- toOps ops, o /= AND, e0 <- es0, e1 <- es1, e0 <= e1]
                esAnd  = [Op2 AND e0 e1 | AND `elem` toOps ops, e0 <- es0, e1 <- es1, e0 <= e1]
                -- (and 0 e) や (and e 0) の形の式は全部同じ意味なので、最初の要素だけを残す
                esAnd' = case filter p esAnd of
                           []  -> esAnd
                           repr:_ -> filter (\e -> not (p e) || e == repr) esAnd
                  where
                    p (Op2 AND (Const Zero) _) = True
                    p (Op2 AND _ (Const Zero)) = True
                    p _ = False
            return $ esAnd' ++ esElse
        ]
      modify (Map.insert (size,fvs) es)
      return es
  where
    toOps :: (Eq a, Enum a, Bounded a, Ord a, Show a) => [String] -> [a]
    toOps xs = [fromJust x | x <- map (flip lookup optbl) xs, isJust x]
      where
        optbl :: (Eq a, Enum a, Bounded a, Ord a, Show a) => [(String, a)]
        optbl = map (map toLower . show &&& id) [minBound..maxBound]

allVars :: [ID]
allVars = ["x" ++ show i | i <- [(1::Int)..]]

module GenProgram.BruteForce (generate) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Data.Set (Set)

import BV
import Interaction

generate :: [String] -> Int -> [Program]
generate ops n = filter (flip isValidFor ops) $ evalStateT (genProgram ops) n

-- 状態は残りサイズ
type Gen = StateT Int []

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
       guard $ e1 <= e2
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

ibind :: Gen a -> (a -> Gen a) -> Gen a
ibind g f = StateT $ \s ->
  interleaveN [runStateT (f a) s' | (a, s') <- runStateT g s]

isum :: [Gen a] -> Gen a
isum gs = StateT $ \s ->
  interleaveN [runStateT g s | g <- gs]

allVars :: [ID]
allVars = ["x" ++ show i | i <- [(1::Int)..]]

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave [] ys = ys

interleaveN :: [[a]] -> [a]
interleaveN [] = []
interleaveN ([]:xss) = interleaveN xss
interleaveN ((x:xs):xss) = x : interleaveN (xss ++ [xs])

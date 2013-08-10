module GenProgram.BruteForce (generate) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Char (toLower)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Data.Set (Set)

import BV
import Interaction

generate :: Int -> [Program]
generate n = evalStateT genProgram n

-- 状態は残りサイズ
type Gen = StateT Int []

genProgram :: Gen Program
genProgram = do
  let (v:vs) = allVars
  consumeSize 1
  e <- genExpr [v] vs
  unused <- get
  guard $ unused == 0
  return $ Program v e

genExpr :: [ID] -> [ID] -> Gen Expr
genExpr fvs unused =
  isum
  [ do consumeSize 1
       msum $ map return $ [Const b | b <- [Zero, One]]
  , do consumeSize 1
       msum $ map return $ [Var v | v <- fvs]
  , do consumeSize 2
       case unused of
         (x:y:unused') -> do
           -- TODO: ibindで対角的に列挙すべき?
           e0 <- genExpr fvs unused
           e1 <- genExpr fvs unused
           e2 <- genExpr (y:x:fvs) unused'
           return $ Fold e0 e1 x y e2
         _ -> mzero
  , do consumeSize 1
       o <- msum $ map return $ [minBound..maxBound]
       e <- genExpr fvs unused
       return $ Op1 o e
  , do consumeSize 1
       o <- msum $ map return $ [minBound..maxBound]
       -- TODO: ibindで対角的に列挙すべき?
       e1 <- genExpr fvs unused
       e2 <- genExpr fvs unused
       return $ Op2 o e1 e2
  ]

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

-- p.size が 最大でも 30 なので、それ以上の変数が必要になることはない
allVars :: [ID]
allVars = [[c] | c <- "xyzabcdefghijklmnopqrstuvw"] ++ [['x',c] | c <- "abcdefghijklmnopqrstuvwxyz"]

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave [] ys = ys

interleaveN :: [[a]] -> [a]
interleaveN [] = []
interleaveN ([]:xss) = interleaveN xss
interleaveN ((x:xs):xss) = x : interleaveN (xss ++ [xs])

toOps :: (Eq a, Enum a, Bounded a, Ord a, Show a) => [String] -> [a]
toOps xs = [fromJust x | x <- map (flip lookup op2tbl) xs, isJust x]
  where
    op2tbl = map (map toLower . show &&& id) [minBound..maxBound]

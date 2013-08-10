module GenProgram.BruteForce (generate) where

import Control.Arrow
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.State
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (toLower)
import Data.List (find)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Data.Set (Set)

import BV
import Interaction

generate' :: Problem -> [Program]
generate' = generate <$> probOperators <*> probSize

generate :: [String] -> Int -> [Program]
generate ops n = evalStateT (genProgram ops) n

-- 状態は残りサイズ
type Gen = StateT Int []

genProgram :: [String] -> Gen Program
genProgram ops = do
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
       case unused of
         (x:y:unused') -> do
           -- TODO: ibindで対角的に列挙すべき?
           e0 <- genExpr ops fvs unused
           e1 <- genExpr ops fvs unused
           e2 <- genExpr ops (y:x:fvs) unused'
           return $ Fold e0 e1 x y e2
         _ -> mzero
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

-- myproblems
myproblems :: IO (Maybe [Problem])
myproblems = fmap (decode . BL.pack) $ readFile "data/myproblems.json"

-- ^ test utility
 -- >>> generateById "5JobhKwrQrnW7ZzR2DUKtQku"
-- [Program "x" (Op1 SHL1 (Const Zero)),Program "x" (Op1 SHL1 (Var "x")),Program "x" (Op1 SHL1 (Const One))]
--
generateById :: String -> IO [Program]
generateById pid = do
  Just ps <- myproblems
  let Just p = find (\p -> probId p == pid) ps
  return $ generate' p


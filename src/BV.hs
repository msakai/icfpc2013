{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module BV
  (
  -- * BV Language
    Program (..)
  , Expr (..)
  , Bin (..)
  , Op1 (..)
  , Op2 (..)
  , ID

  -- * Semantics
  , Value
  , Env
  , eval

  -- * Metadata
  , Measureable (..)
  , ToSet (..)
  , isValidFor

  -- * Pretty Printing
  , Render (..)

  -- * Parsing
  , program
  , parseProgram
  , parseExpr
  
  -- * User-friendly combinators
  , var
  , b0, b1
  , if0
  , fold
  , not', shl1, shr1, shr4, shr16
  , and', or', xor', plus
  ) where

import Control.Monad
import Data.Char
import Data.Bits
import Data.Char (toLower)
import Data.List (union, (\\))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Word

import SExp

{--------------------------------------------------------------------
  BV Language
--------------------------------------------------------------------}

data Program = Program ID Expr
  deriving (Eq, Ord, Show)

data Expr
  = Const Bin
  | Var ID
  | If0 Expr Expr Expr
  | Fold Expr Expr ID ID Expr
  | Op1 Op1 Expr
  | Op2 Op2 Expr Expr
  deriving (Eq, Ord, Show)

data Bin = Zero | One
  deriving (Eq, Ord, Show, Bounded, Enum)

data Op1 = NOT | SHL1 | SHR1 | SHR4 | SHR16
  deriving (Eq, Ord, Show, Bounded, Enum)

data Op2 = AND | OR | XOR | PLUS
  deriving (Eq, Ord, Show, Bounded, Enum)

type ID = String

{--------------------------------------------------------------------
  Semantics
--------------------------------------------------------------------}

type Value = Word64

type Env = Map ID Value

eval :: Program -> Value -> Value
eval (Program arg body) val = eval1 (Map.singleton arg val) body

eval1 :: Env -> Expr -> Value
eval1 env (Const b) =
  case b of
    One  -> 1
    Zero -> 0
eval1 env (Var id) = env Map.! id
eval1 env (If0 c t e) =
  if eval1 env c == 0
  then eval1 env t
  else eval1 env e
eval1 env (Fold e0 e1 x y e2) = foldr phi v1 vs
  where
    v0 = eval1 env e0
    v1 = eval1 env e1
    vs = [(v0 `shiftR` s) .&. 0xFF | s <- [56,48..0]]
    phi vx vy = eval1 env' e2
      where
        env' = Map.insert x vx $ Map.insert y vy env
eval1 env (Op1 op e)     = evalOp1 op (eval1 env e)
eval1 env (Op2 op e1 e2) = evalOp2 op (eval1 env e1) (eval1 env e2)

evalOp1 :: Op1 -> Value -> Value
evalOp1 NOT   = complement
evalOp1 SHL1  = (`shiftL` 1)
evalOp1 SHR1  = (`shiftR` 1)
evalOp1 SHR4  = (`shiftR` 4)
evalOp1 SHR16 = (`shiftR` 16)

evalOp2 :: Op2 -> Value -> Value -> Value
evalOp2 AND  = (.&.)
evalOp2 OR   = (.|.)
evalOp2 XOR  = xor
evalOp2 PLUS = (+)

{--------------------------------------------------------------------
  Metadata
--------------------------------------------------------------------}

-- | size is method as |.|
class Measureable a where
  size :: a -> Int

instance Measureable Expr where
  size (Const _)            = 1
  size (Var _)              = 1
  size (If0 e0 e1 e2)       = 1 + size e0 + size e1 + size e2
  size (Fold e0 e1 x y e2)  = 2 + size e0 + size e1 + size e2
  size (Op1 _ e0)           = 1 + size e0
  size (Op2 _ e0 e1)        = 1 + size e0 + size e1

instance Measureable Program where
  size (Program x e)        = 1 + size e

class ToSet a where
  op :: a -> [String]

instance ToSet Expr where
  op (Const _) = []
  op (Var _) = []
  op (If0 e0 e1 e2) = ["if0"] `union` op e0 `union` op e1 `union` op e2
  op (Fold e0 e1 x y e2) = ["fold"] `union` op e0 `union` op e1 `union` op e2
  op (Op1 o e0) = [map toLower $ show o] `union` op e0
  op (Op2 o e0 e1) = [map toLower $ show o] `union` op e0 `union` op e1

instance ToSet Program where
  op (Program x (Fold (Var x') (Const Zero) y z e)) = ["tfold"] `union` op e
  op (Program _ e) = op e

-- | Operators constraints
isValidFor :: Program -> [String] -> Bool
p `isValidFor` ops = null (op p \\ ops) && null (ops \\ op p)

{--------------------------------------------------------------------
  Pretty Printing
--------------------------------------------------------------------}

class Render a where
  render :: a -> String

instance Render Program where
  render = renderSExp . toSExp

instance Render Expr where
  render = renderSExp . toSExp

instance ToSExp Program where
  toSExp (Program x body) =
    SApply [ SAtom "lambda"
           , SApply [SAtom x]
           , toSExp body
           ]

instance ToSExp Expr where
  toSExp (Const b)   = toSExp b
  toSExp (Var x)     = toSExp x
  toSExp (If0 c t e) = SApply [SAtom "if0", toSExp c, toSExp t, toSExp e]
  toSExp (Fold e0 e1 x y e2) =
    SApply [ SAtom "fold"
           , toSExp e0
           , toSExp e1
           , SApply [ SAtom "lambda"
                    , SApply [SAtom x, SAtom y]
                    , toSExp e2
                    ]
           ]
  toSExp (Op1 op e)     = SApply [toSExp op, toSExp e]
  toSExp (Op2 op e1 e2) = SApply [toSExp op, toSExp e1, toSExp e2]

instance ToSExp Bin where
  toSExp Zero = SAtom "0"
  toSExp One  = SAtom "1"

instance ToSExp ID where
  toSExp v = SAtom v

instance ToSExp Op1 where
  toSExp = SAtom . map toLower . show

instance ToSExp Op2 where
  toSExp = SAtom . map toLower . show

{--------------------------------------------------------------------
  Parsing
--------------------------------------------------------------------}

program :: String -> Program
program = fromJust . parseProgram

parseProgram :: String -> Maybe Program
parseProgram s = fromSExp =<< parseSExp s

parseExpr :: String -> Maybe Expr
parseExpr s = fromSExp =<< parseSExp s

instance FromSExp Program where
  fromSExp (SApply [SAtom "lambda", SApply [SAtom x], body]) = do
    e <- fromSExp body
    return $ Program x e
  fromSExp _ = mzero

instance FromSExp Expr where
  fromSExp s =
    msum
    [ liftM Const (fromSExp s)
    , case s of
        SApply [SAtom "if0", c, t, e] -> do
          c' <- fromSExp c
          t' <- fromSExp t
          e' <- fromSExp e
          return $ If0 c' t' e'
        _ -> mzero
    , case s of
        SApply [SAtom "fold", e0, e1, SApply [SAtom "lambda", SApply [SAtom x, SAtom y], e2]] -> do
          e0' <- fromSExp e0
          e1' <- fromSExp e1
          e2' <- fromSExp e2
          return $ Fold e0' e1' x y e2'
        _ -> mzero
    , case s of
        SApply [op, e] -> do
          op' <- fromSExp op
          e'  <- fromSExp e
          return $ Op1 op' e'
        _ -> mzero
    , case s of
        SApply [op, e1, e2] -> do
          op' <- fromSExp op
          e1' <- fromSExp e1
          e2' <- fromSExp e2
          return $ Op2 op' e1' e2'
        _ -> mzero
    , liftM Var (fromSExp s) -- IDはワイルドカード的になってしまうので意図的に最後に
    ]

instance FromSExp ID where
  fromSExp (SAtom x) = return x
  fromSExp _ = mzero

instance FromSExp Bin where
  fromSExp (SAtom "0") = return Zero
  fromSExp (SAtom "1") = return One
  fromSExp _ = mzero

instance FromSExp Op1 where
  fromSExp (SAtom x) = listToMaybe [op | op <- [minBound..maxBound], x == map toLower (show op)]
  fromSExp _ = mzero

instance FromSExp Op2 where
  fromSExp (SAtom x) = listToMaybe [op | op <- [minBound..maxBound], x == map toLower (show op)]
  fromSExp _ = mzero

{--------------------------------------------------------------------
  User-friendly combinators
--------------------------------------------------------------------}

var :: String -> Expr
var = Var

b0, b1 :: Expr
b0 = Const Zero
b1 = Const One

if0 :: Expr -> Expr -> Expr -> Expr
if0 = If0

fold :: Expr -> Expr -> ID -> ID -> Expr -> Expr
fold = Fold

not', shl1, shr1, shr4, shr16 :: Expr -> Expr
not'  = Op1 NOT
shl1  = Op1 SHL1
shr1  = Op1 SHR1
shr4  = Op1 SHR4
shr16 = Op1 SHR16

and', or', xor', plus :: Expr -> Expr -> Expr
and' = Op2 AND
or'  = Op2 OR
xor' = Op2 XOR
plus = Op2 PLUS

module BV where

import Data.Bits
import Data.List (union)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Word

data Program
  = Program
  { pArg  :: ID
  , pBody :: Expr
  }
  deriving (Eq, Ord, Show)

data Expr
  = Const Bin
  | Id ID
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

type Value = Word64

type Env = Map ID Value

eval :: Program -> Value -> Value
eval (Program arg body) val = eval1 (Map.singleton arg val) body

eval1 :: Env -> Expr -> Value
eval1 env (Const b) =
  case b of
    One  -> 1
    Zero -> 0
eval1 env (Id id) = env Map.! id
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


-- ^ Metadata
-- size is method as |.|
class Measureable a where
  size :: a -> Int

instance Measureable Expr where
  size (Const _)            = 1
  size (Id _)               = 1
  size (If0 e0 e1 e2)       = 1 + size e0 + size e1 + size e2
  size (Fold e0 e1 x y e2)  = 2 + size e0 + size e1 + size e2
  size (Op1 _ e0)           = 1 + size e0
  size (Op2 _ e0 e1)        = 1 + size e0 + size e1

instance Measureable Program where
  size (Program x e)        = 1 + size e

-- ^ Op function
op :: Expr -> [Ops]
op (Const _) = []
op (Id _) = []
op (If0 e0 e1 e2) = [OpsIf0] `union` op e0 `union` op e1 `union` op e2
op (Fold e0 e1 x y e2) = [OpsFold] `union` op e0 `union` op e1 `union` op e2
op (Op1 o e0) = [OpsOp1 o] `union` op e0
op (Op2 o e0 e1) = [OpsOp2 o] `union` op e0 `union` op e1

data Ops = OpsOp1 Op1
         | OpsOp2 Op2
         | OpsIf0
         | OpstFold
         | OpsFold
         deriving (Eq, Ord, Show)

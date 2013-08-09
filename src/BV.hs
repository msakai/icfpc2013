module BV where

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
  deriving (Eq, Ord, Show)

data Op1 = NOT | SHL1 | SHR1 | SHR4 | SHR16
  deriving (Eq, Ord, Show)

data Op2 = AND | OR | XOR | PLUS
  deriving (Eq, Ord, Show)

type ID = String


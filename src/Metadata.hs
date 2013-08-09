module Metadata where

import Data.List (union)
import BV

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
op (Const _) = []
op (Id _) = []
op (If0 e0 e1 e2) = ["if0"] `union` op e0 `union` op e1 `union` op e2
op (Fold e0 e1 x y e2) = ["fold"] `union` op e0 `union` op e1 `union` op e2
op (Op1 o e0) = [show o] `union` op e0
op (Op2 o e0 e1) = [show o] `union` op e0 `union` op e1

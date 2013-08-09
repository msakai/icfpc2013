module Metadata where

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

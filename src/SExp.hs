module SExp where

import Data.List (intersperse)

data SExp
  = SApply [SExp]
  | SAtom String

renderSExp :: SExp -> String
renderSExp e = f e ""
  where
    f :: SExp -> ShowS
    f (SAtom str) = showString str
    f (SApply xs) = showChar '(' . foldr (.) id gs . showChar ')'
      where
        gs = intersperse (showChar ' ') [f e | e <- xs]

class ToSExp a where
  toSExp :: a -> SExp

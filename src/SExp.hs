module SExp
  ( SExp (..)
  , renderSExp
  , sexp
  , parseSExp
  , ToSExp (..)
  , FromSExp (..)
  ) where

import Control.Monad
import Data.Char
import Data.List (intersperse)
import Data.Maybe
import Text.ParserCombinators.Parsec

data SExp
  = SApply [SExp]
  | SAtom String
  deriving (Eq, Ord, Show)

renderSExp :: SExp -> String
renderSExp e = f e ""
  where
    f :: SExp -> ShowS
    f (SAtom str) = showString str
    f (SApply xs) = showChar '(' . foldr (.) id gs . showChar ')'
      where
        gs = intersperse (showChar ' ') [f e | e <- xs]

parseSExp :: String -> Maybe SExp
parseSExp s =
  case parse (spaces >> sexp') "-" s of
    Left err -> Nothing
    Right a -> Just a

sexp :: String -> SExp
sexp = fromJust . parseSExp

sexp' :: Parser SExp
sexp' = do
  s <- (app `mplus` atom)
  spaces
  return s

app :: Parser SExp
app = 
  liftM SApply $
    between (char '(' >> spaces) (char ')' >> spaces) $ 
      many sexp'

atom :: Parser SExp
atom = liftM SAtom $ many1 (satisfy (\x -> x /= '(' && x /= ')' && not (isSpace x)))

class ToSExp a where
  toSExp :: a -> SExp

class FromSExp a where
  fromSExp :: SExp -> Maybe a


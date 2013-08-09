{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Test.HUnit hiding (Test)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import BV

case_test1 = do
  eval prog 0x1122334455667788 @?= 0xFF
  where
    -- P = (lambda (x) (fold x 0 (lambda (y z) (or y z))))
    prog = Program "x" (Fold (Id "x") (Const Zero) "y" "z" (Op2 OR (Id "y") (Id "z")))

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)

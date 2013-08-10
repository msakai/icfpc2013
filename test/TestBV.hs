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
    prog = Program "x" (fold (var "x") b0 "y" "z" (or' (var "y") (var "z")))

case_test2 = do
  eval prog 0 @?= 0x00000000000055D5
  eval prog 1 @?= 0x00000000000054D5
  eval prog 12345 @?= 0x00000000012310D5
  where
    -- P = (lambda (x_65671) (fold (shr1 (or (not (xor (plus x_65671 (shr1 (if0 (or (plus (shl1 x_65671) x_65671) x_65671) 0 1))) x_65671)) 0)) x_65671 (lambda (x_65672 x_65673) (xor (shl1 x_65673) x_65672))))
    prog =
      Program "x_65671" $
        Fold (shr1 (or' (not' (xor' (plus (var "x_65671") (shr1 (if0 (or' (plus (shl1 (var "x_65671")) (var "x_65671")) (var "x_65671")) b0 b1))) (var "x_65671"))) b0))
             (var "x_65671")
             "x_65672" "x_65673" (xor' (shl1 (var "x_65673")) (var "x_65672"))

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)

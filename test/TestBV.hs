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
  eval prog 0 @?= 0x0000000000000000
  eval prog 1 @?= 0x0000000000000001
  eval prog 2 @?= 0x0000000000000002
  eval prog 3 @?= 0x0000000000000003
  eval prog 4 @?= 0x0000000000000004
  eval prog 5 @?= 0x0000000000000005
  eval prog 0x112233 @?= 0x0000000000000033
  eval prog 0x1122334455667788 @?= 0x00000000000000FF
  eval prog 0xFFFFFFFFFFFFFFFF @?= 0x00000000000000FF
  where
    -- P = (lambda (x) (fold x 0 (lambda (y z) (or y z))))
    prog = Program "x" (fold x b0 "y" "z" (or' y z))
    x = var "x"
    y = var "y"
    z = var "z"

case_test2 = do
  eval prog 0 @?= 0x00000000000055D5
  eval prog 1 @?= 0x00000000000054D5
  eval prog 0x12345 @?= 0x00000000012310D5
  where
    -- P = (lambda (x_65671) (fold (shr1 (or (not (xor (plus x_65671 (shr1 (if0 (or (plus (shl1 x_65671) x_65671) x_65671) 0 1))) x_65671)) 0)) x_65671 (lambda (x_65672 x_65673) (xor (shl1 x_65673) x_65672))))
    prog =
      Program "x_65671" $
        Fold (shr1 (or' (not' (xor' (plus x_65671 (shr1 (if0 (or' (plus (shl1 x_65671) x_65671) x_65671) b0 b1))) x_65671)) b0))
             x_65671
             "x_65672" "x_65673" (xor' (shl1 x_65673) x_65672)
    x_65671 = var "x_65671"
    x_65672 = var "x_65672"
    x_65673 = var "x_65673"

case_not = do
  eval prog 0 @?= 0xFFFFFFFFFFFFFFFF
  eval prog 1 @?= 0xFFFFFFFFFFFFFFFE
  eval prog 2 @?= 0xFFFFFFFFFFFFFFFD
  eval prog 3 @?= 0xFFFFFFFFFFFFFFFC
  eval prog 4 @?= 0xFFFFFFFFFFFFFFFB
  eval prog 5 @?= 0xFFFFFFFFFFFFFFFA 
  eval prog 0x112233 @?= 0xFFFFFFFFFFEEDDCC
  eval prog 0x1122334455667788 @?= 0xEEDDCCBBAA998877
  eval prog 0xFFFFFFFFFFFFFFFF @?= 0x0000000000000000
  where
    -- P = (lambda (x) (not x))
    prog = Program "x" (not' x)
    x = var "x"

case_shl1 = do
  forM_ (zip inputs outputs) $ \(x,y) -> do
    eval prog x @?= y
  where
    prog = Program "x" (shl1 x)
    x = var "x"
    inputs  = [0x0,0x1,0x2,0x3,0x4,0x5,0x112233,0x1122334455667788,0xFFFFFFFFFFFFFFFF]
    outputs = [0x0000000000000000, 0x0000000000000002, 0x0000000000000004, 0x0000000000000006, 0x0000000000000008, 0x000000000000000A, 0x0000000000224466, 0x22446688AACCEF10, 0xFFFFFFFFFFFFFFFE]

case_shr1 = do
  forM_ (zip inputs outputs) $ \(x,y) -> do
    eval prog x @?= y
  where
    prog = Program "x" (shr1 x)
    x = var "x"
    inputs  = [0x0,0x1,0x2,0x3,0x4,0x5,0x112233,0x1122334455667788,0xFFFFFFFFFFFFFFFF]
    outputs = [0x0000000000000000, 0x0000000000000000, 0x0000000000000001, 0x0000000000000001, 0x0000000000000002, 0x0000000000000002, 0x0000000000089119, 0x089119A22AB33BC4, 0x7FFFFFFFFFFFFFFF]

case_shr4 = do
  forM_ (zip inputs outputs) $ \(x,y) -> do
    eval prog x @?= y
  where
    prog = Program "x" (shr4 x)
    x = var "x"
    inputs  = [0x0,0x1,0x2,0x3,0x4,0x5,0x112233,0x1122334455667788,0xFFFFFFFFFFFFFFFF]
    outputs = [0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000011223, 0x0112233445566778, 0x0FFFFFFFFFFFFFFF]

case_shr16 = do
  forM_ (zip inputs outputs) $ \(x,y) -> do
    eval prog x @?= y
  where
    prog = Program "x" (shr16 x)
    x = var "x"
    inputs  = [0x0,0x1,0x2,0x3,0x4,0x5,0x112233,0x1122334455667788,0xFFFFFFFFFFFFFFFF]
    outputs = [0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000011, 0x0000112233445566, 0x0000FFFFFFFFFFFF]

case_xor = do
  forM_ (zip inputs outputs) $ \(x,y) -> do
    eval prog x @?= y
  where
    -- (lambda (x) (xor (shl1 x) x))
    prog = Program "x" (xor' (shl1 x) x)
    x = var "x"
    inputs  = [0x0,0x1,0x2,0x3,0x4,0x5,0x12345,0x112233,0x1122334455667788,0xFFFFFFFFFFFFFFFF]
    outputs = [0x0000000000000000, 0x0000000000000003, 0x0000000000000006, 0x0000000000000005, 0x000000000000000C, 0x000000000000000F, 0x00000000000365CF, 0x0000000000336655, 0x336655CCFFAA9898, 0x0000000000000001]

case_if0 = do
  forM_ (zip inputs outputs) $ \(x,y) -> do
    eval prog x @?= y
  where
    prog = Program "x" (if0 x b1 b0)
    x = var "x"
    inputs  = [0x0,0x1,0x2,0x3,0x4,0x5,0x112233,0x1122334455667788,0xFFFFFFFFFFFFFFFF]
    outputs = [0x0000000000000001, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000]

case_fold = do
  forM_ (zip inputs outputs) $ \(x,y) -> do
    eval prog x @?= y
  where
    -- (lambda (x) (fold x 1 (lambda (y z) (xor y (shl1 z)))))
    prog = Program "x" (fold x b1 "y" "z" (xor' y (shl1 z)))
    x = var "x"
    y = var "y"
    z = var "z"
    inputs  = [0x0,0x1,0x2,0x3,0x4,0x5,0x112233,0x1122334455667788,0xFFFFFFFFFFFFFFFF]
    outputs = [0x0000000000000100, 0x0000000000000180, 0x0000000000000000, 0x0000000000000080, 0x0000000000000300, 0x0000000000000380, 0x0000000000001220, 0x00000000000053E9, 0x0000000000005455]

case_fold_2 = do
  forM_ (zip inputs outputs) $ \(x,y) -> do
    eval prog x @?= y
  where
    -- (lambda (x) (fold x 0 (lambda (y z) y)))
    prog = Program "x" (fold x b0 "y" "z" y)
    x = var "x"
    y = var "y"
    z = var "z"
    inputs  = [0x0,0x1,0x2,0x3,0x4,0x5,0x12345,0x112233,0x1122334455667788,0xFFFFFFFFFFFFFFFF]
    outputs = [0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000011, 0x00000000000000FF]

case_fold_3 = do
  forM_ (zip inputs outputs) $ \(x,y) -> do
    eval prog x @?= y
  where
    -- (lambda (x) (fold x 0 (lambda (y z) z)))
    prog = Program "x" (fold x b0 "y" "z" z)
    x = var "x"
    y = var "y"
    z = var "z"
    inputs  = [0x0,0x1,0x2,0x3,0x4,0x5,0x12345,0x112233,0x1122334455667788,0xFFFFFFFFFFFFFFFF]
    outputs = [0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x0000000000000000]

case_parseProgram =
  parseProgram "(lambda (x) (fold x 0 (lambda (y z) (or y z))))" @?= Just expected
  where
    expected = Program "x" (Fold (Var "x") (Const Zero) "y" "z" (Op2 OR (Var "y") (Var "z")))

case_parseExpr =
  parseExpr "(fold x 0 (lambda (y z) (or y z)))" @?= Just expected
  where
    expected = Fold (Var "x") (Const Zero) "y" "z" (Op2 OR (Var "y") (Var "z"))

case_renderProgram = render prog @?= "(lambda (x) (fold x 0 (lambda (y z) (or y z))))"
  where
    prog = Program "x" (Fold (Var "x") (Const Zero) "y" "z" (Op2 OR (Var "y") (Var "z")))

case_renderExpr = render e @?= "(fold x 0 (lambda (y z) (or y z)))"
  where
    e = Fold (Var "x") (Const Zero) "y" "z" (Op2 OR (Var "y") (Var "z"))

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)

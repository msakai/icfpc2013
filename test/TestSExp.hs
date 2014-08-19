{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Test.HUnit hiding (Test)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import SExp

case_test1 = do
  parseSExp "(lambda (x) (fold x 0 (lambda (y z) (or y z))))" @?= Just expected
  where
    expected =
      SApply [ SAtom "lambda"
             , SApply [SAtom "x"]
             , SApply [ SAtom "fold"
                      , SAtom "x"
                      , SAtom "0" 
                      , SApply [ SAtom "lambda"
                               , SApply [SAtom "y", SAtom "z"]
                               , SApply [SAtom "or", SAtom "y", SAtom "z"]
                               ]
                      ]
             ]

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)

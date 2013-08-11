module SMT
  ( prelude
  , defineProgram
  , genFindDiff

  , defineTemplate
  , genCheckTemplateSAT
  , checkTemplateSAT
  ) where

import Control.Monad.RWS
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import System.Directory
import System.Exit
import System.IO
import System.Process
import System.IO.Unsafe

import BV
import SExp
import qualified GenProgram.Template as T

type Gen = RWS (Set ID) [SExp] Int
-- Set ID は現在スコープ内にある変数の集合
-- [SExp] は生成した補助定義のリスト
-- Intはカウンター

defineProgram :: String -> Program -> [SExp]
defineProgram name (Program x e) =
  case runRWS (encodeExpr name e) (Set.singleton x) 0 of
    (e, _, defs) ->
      defs ++ [SApply [SAtom "define-fun", SAtom name, SApply [SApply [SAtom x, valueSort]], valueSort, e]]
      -- (define-fun name ((x (_ BitVec 64))) (_ BitVec 64) e)

encodeExpr :: String -> Expr -> Gen SExp
encodeExpr = encodeExpr' (\id -> return $ SAtom id)

encodeExpr' :: (ID -> Gen SExp) -> String -> Expr -> Gen SExp
encodeExpr' handleVar prefix = f
  where
    f (Const Zero) = return zero
    f (Const One)  = return one
    f (Var id) = handleVar id
    f (If0 c t e) = do
      c' <- f c
      t' <- f t
      e' <- f e
      return $ SApply [SAtom "if", SApply [SAtom "=", c', zero], t', e']
    f (Fold e0 e1 y z e2) = do
      fname <- gensym prefix
      fvs <- ask
      let fvs' = fvs `Set.difference` Set.fromList [y,z]
          params = Set.toAscList fvs' ++ [y, z]
      e2' <- local (Set.insert y . Set.insert z) $ f e2
      tell $ [SApply [ SAtom "define-fun", fname, SApply [SApply [SAtom v, valueSort] | v <- params], valueSort, e2' ]]

      e0' <- f e0 -- リスト相当
      e1' <- f e1 -- 初期値
      
      tmp <- gensym prefix
      let zero56 = SAtom "#b00000000000000000000000000000000000000000000000000000000"
          -- "(concat zero56 ((_ extract 63 56) tmp))"
          a8 = SApply [SAtom "concat", zero56, SApply [SApply [SAtom "_", SAtom "extract", SAtom "63", SAtom "56"], tmp]]
          a7 = SApply [SAtom "concat", zero56, SApply [SApply [SAtom "_", SAtom "extract", SAtom "55", SAtom "48"], tmp]]
          a6 = SApply [SAtom "concat", zero56, SApply [SApply [SAtom "_", SAtom "extract", SAtom "47", SAtom "40"], tmp]]
          a5 = SApply [SAtom "concat", zero56, SApply [SApply [SAtom "_", SAtom "extract", SAtom "39", SAtom "32"], tmp]]
          a4 = SApply [SAtom "concat", zero56, SApply [SApply [SAtom "_", SAtom "extract", SAtom "31", SAtom "24"], tmp]]
          a3 = SApply [SAtom "concat", zero56, SApply [SApply [SAtom "_", SAtom "extract", SAtom "23", SAtom "16"], tmp]]
          a2 = SApply [SAtom "concat", zero56, SApply [SApply [SAtom "_", SAtom "extract", SAtom "15", SAtom  "8"], tmp]]
          a1 = SApply [SAtom "concat", zero56, SApply [SApply [SAtom "_", SAtom "extract", SAtom  "7", SAtom  "0"], tmp]]
          body = foldr (\a b -> SApply $ fname : [SAtom v | v <- Set.toList fvs'] ++ [a, b]) e1' [a8,a7,a6,a5,a4,a3,a2,a1]

      return $ SApply [ SAtom "let", SApply [SApply [tmp, e0']], body ]
    f (Op1 op e) = do
      e'  <- f e
      return $ SApply [encodeOp1 op, e']
    f (Op2 op e1 e2) = do
      e1' <- f e1
      e2' <- f e2
      return $ SApply [encodeOp2 op, e1', e2']

gensym :: String -> Gen SExp
gensym prefix = do
  i <- get
  put $! i+1
  return $ SAtom $ prefix ++ "-" ++ show i

encodeOp1 :: Op1 -> SExp
encodeOp1 NOT   = SAtom $ "bvnot"
encodeOp1 SHL1  = SAtom $ "shl1"
encodeOp1 SHR1  = SAtom $ "shr1"
encodeOp1 SHR4  = SAtom $ "shr4"
encodeOp1 SHR16 = SAtom $ "shr16"

encodeOp2 :: Op2 -> SExp
encodeOp2 AND  = SAtom $ "bvand"
encodeOp2 OR   = SAtom $ "bvor"
encodeOp2 XOR  = SAtom $ "bvxor"
encodeOp2 PLUS = SAtom $ "bvadd"

encodeValue :: Value -> SExp
encodeValue i = SApply [SAtom "_", SAtom ("bv" ++ show i), SAtom "64"]

prelude :: [SExp]
prelude =
  [ sexp "(define-fun shl1 ((x (_ BitVec 64))) (_ BitVec 64) (concat ((_ extract 62 0) x) #b0))"
  , sexp "(define-fun shr1 ((x (_ BitVec 64))) (_ BitVec 64) (concat #b0 ((_ extract 63 1) x)))"
  , sexp "(define-fun shr4 ((x (_ BitVec 64))) (_ BitVec 64) (concat #b0000 ((_ extract 63 4) x)))"
  , sexp "(define-fun shr16 ((x (_ BitVec 64))) (_ BitVec 64) (concat #b0000000000000000 ((_ extract 63 16) x)))"
  ]

valueSort :: SExp
valueSort = SApply [SAtom "_", SAtom "BitVec", SAtom "64"]

zero :: SExp
zero = SAtom "#b0000000000000000000000000000000000000000000000000000000000000000"

one :: SExp
one = SAtom "#b0000000000000000000000000000000000000000000000000000000000000001"

-- ---------------------------------------------------------------------------

genFindDiff :: Program -> Program -> [SExp]
genFindDiff prog1 prog2 =
  concat
  [ map sexp ["(set-logic QF_BV)", "(set-option :produce-assignments true)"] -- (set-option :produce-models true) ?
  , prelude
  , defineProgram "program1" prog1
  , defineProgram "program2" prog2
  , map sexp ["(declare-fun x () (_ BitVec 64))","(assert (not (= (program1 x) (program2 x))))","(check-sat)","(get-value (x))"]
  ]

test_prog1 = program "(lambda (x) (fold x 0 (lambda (y z) (or y z))))"
test_prog2 = program "(lambda (x) (fold x 0 (lambda (y z) (and y z))))"

test = unlines $ map renderSExp $ genFindDiff test_prog1 test_prog2
{-
これで生成されるものを test.smt2 というファイルに保存して、以下のように実行する。

$ cvc4 test.smt2
sat
((x (_ bv72057594037927936 64)))
$

二つのプログラムが振る舞いの異なるプログラムの場合には sat と出力され、
次に反例となる入力値の情報が出力される。
ここでは 72057594037927936 が反例となる値。

CVC4は http://cvc4.cs.nyu.edu/web/ からダウンロード・インストールのこと。

Z3 http://z3.codeplex.com/ でもOKだが、値の表示方法が異なるので注意。
Z3オンラインでも試せる: http://rise4fun.com/z3
-}

-- ---------------------------------------------------------------------------

defineTemplate :: String -> T.Template -> [SExp]
defineTemplate name t =
  case runRWS (encodeExpr' handleVar name e) (Set.singleton x) 0 of
    (e, _, defs) ->
      defs ++ [SApply [SAtom "define-fun", SAtom name, SApply [SApply [SAtom x, valueSort]], valueSort, e]]
      -- (define-fun name ((x (_ BitVec 64))) (_ BitVec 64) e)
  where
    Program x e = T.tProgram t

    handleVar id =
      case Map.lookup id (T.holesInfo t) of
        Nothing     -> return $ SAtom id
        Just (vs,_) -> do
          let fname = SAtom $ name ++ "-" ++ id
          tell $ [SApply [ SAtom "declare-fun", fname, SApply [valueSort | _ <- vs], valueSort]]
          return $ SApply (fname : [SAtom v | v <- vs])

genCheckTemplateSAT :: T.Template -> [(Value, Value)] -> [SExp]
genCheckTemplateSAT t ios =
  concat
  [ map sexp ["(set-logic QF_UFBV)"]
  , prelude
  , defineTemplate "program1" T.emptyTFold
  , [SApply [SAtom "assert", SApply [SAtom "=", SApply [SAtom "program1", encodeValue i], encodeValue o]] | (i,o) <- ios]
  , map sexp ["(check-sat)"]
  ]

checkTemplateSAT :: T.Template -> [(Value, Value)] -> Bool
checkTemplateSAT t ios = unsafePerformIO $ do
  tmpdir <- getTemporaryDirectory
  (fname, h) <- openTempFile tmpdir "checkTemplateSAT.smt2"

  let src = unlines $ map renderSExp $ genCheckTemplateSAT t ios
  hPutStr h src
  hClose h

  r@(ret,_,_) <- readProcessWithExitCode "cvc4" [fname] []
  hPrint stderr r
  removeFile fname

  -- Segmentation Fault
  when (ret == ExitFailure 6) $ do
    hPutStrLn stderr src

  -- http://cvc4.cs.nyu.edu/wiki/User_Manual#Exit_status
  if ret == ExitFailure 20
  then return False
  else return True -- それ以外は安全側でsatに倒す

-- ---------------------------------------------------------------------------

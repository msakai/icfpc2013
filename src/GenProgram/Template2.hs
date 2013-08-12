module Template2 where

import Control.Monad
import Control.Monad.RWS
import Control.Monad.State
import Data.List
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

-- ---------------------------------------------------------------------------------------------

type HoleID = Int

data Template
  = TempNormal ID TExpr
  | TempTFold ID ID TExpr
  | TempBonus ID TExpr TExpr TExpr

data TExpr
  = TLeaf HoleID [ID]
  | TIf0 TExpr TExpr TExpr
  | TFold TExpr TExpr ID ID TExpr
  | TOp1 HoleID [Op1] TExpr
  | TOp2 HoleID [Op2] TExpr TExpr
  deriving (Show)

type Gen = State (Map (Int,[ID]) [TExpr])

generateTExprs :: Int -> ([ID],[ID]) -> [String] -> [TExpr]
generateTExprs size info ops = evalState (genTExprs size info ops) Map.empty

genTExprs :: Int -> ([ID],[ID]) -> [String] -> Gen [TExpr]
genTExprs size _ _ | size <= 0 = return []
genTExprs size (fvs,unused) ops = do
  tbl <- get
  case Map.lookup (size, fvs) tbl of
    Just xs -> return xs
    Nothing -> do  
      xs <- liftM concat $ sequence $
        [ return [TLeaf 0 fvs | size == 1]
        , liftM concat $ forM [(s0,s1) | "if0" `elem` ops, s0 <- [1..size-1], s1 <- [1..size-1-s0]] $ \(s0,s1) -> do
            let s2 = size-1-s0-s1
            es0 <- genTExprs s0 (fvs,unused) ops
            es1 <- genTExprs s1 (fvs,unused) ops
            es2 <- genTExprs s2 (fvs,unused) ops
            return [TIf0 e0 e1 e2 | e0 <- es0, e1 <- es1, e2 <- es2]
        , liftM concat $ forM [(s0,s1) | "fold" `elem` ops, s0 <- [1..size-2], s1 <- [1..size-2-s0]] $ \(s0,s1) -> do
            let s2 = size-2-s0-s1
            es0 <- genTExprs s0 (fvs,unused) ops
            es1 <- genTExprs s1 (fvs,unused) ops
            let v1:v2:unused' = unused
                fvs' = v2 : v1 : fvs
            es2 <- genTExprs s2 (fvs', unused') ops
            return [TFold e0 e1 v1 v2 e2 | e0 <- es0, e1 <- es1, e2 <- es2]
        , if null op1s
          then return []
          else do es <- genTExprs (size-1) (fvs,unused) ops
                  return [TOp1 0 op1s e | e <- es]
        , if null op2s
          then return []
          else do
            liftM concat $ forM [1..size-1] $ \s0 -> do
              let s1 = size-1-s0
              es0 <- genTExprs s0 (fvs,unused) ops
              es1 <- genTExprs s1 (fvs,unused) ops
              return [TOp2 0 op2s e0 e1 | e0 <- es0, e1 <- es1]
        ]
      modify (Map.insert (size,fvs) xs)
      return xs
  where
    op1s = [o | o <- [(minBound::Op1)..maxBound], render o `elem` ops]
    op2s = [o | o <- [(minBound::Op2)..maxBound], render o `elem` ops]

allVars :: [ID]
allVars = ["x" ++ show i | i <- [(1::Int)..]]

-- ---------------------------------------------------------------------------------------------

data ProgramSet
  = ProgramSet
  { psTemplates :: [Template]
  , psOps       :: [String]
  , psExamples  :: [(Value, Value)]
  }

generate :: [String] -> Int -> ProgramSet
generate ops n
  | "tfold" `elem` ops =
      let (x:y:vs) = allVars
      in ProgramSet
         { psTemplates  = [labelTemplate' (TempTFold x y e) | e <- generateTExprs (n-5) ([y,x],vs) ops]
         , psOps = ops
         , psExamples = []
         }
  | otherwise = 
      let (x:vs) = allVars
      in ProgramSet
         { psTemplates = [labelTemplate' (TempNormal x e) | e <- generateTExprs (n-1) ([x],vs) ops]
         , psOps = ops
         , psExamples = []
         }

pickup :: ProgramSet -> Maybe Program
pickup ps = msum $ map (pickupFromTemplate (psOps ps) (psExamples ps)) $ psTemplates ps

pickupFromTemplate :: [String] -> [(Value,Value)] -> Template -> Maybe Program
pickupFromTemplate ops ios t = unsafePerformIO $ do
  let src = unlines $ map renderSExp $ concat $ 
            [ map sexp ["(set-logic QF_BV)", "(set-option :produce-assignments true)"]
            , prelude
            , defineProgram "program" t
            , [ SApply [SAtom "assert", SApply [SAtom "=", SApply [SAtom "program", encodeValue i], encodeValue o]] | (i,o) <- ios ]
            , map sexp ["(check-sat)"]
            ]
  putStrLn src

  tmpdir <- getTemporaryDirectory
  (fname, h) <- openTempFile tmpdir "pickupFromTemplate.smt2"
  hPutStr h src
  hClose h

  r@(ret,_,_) <- readProcessWithExitCode "cvc4" [fname] []
  hPrint stderr r  

  removeFile fname
  
  undefined

type SMTGen = RWS (Set ID) [SExp] (Int, [(HoleID, [(SExp, String)])])
-- Set ID は現在スコープ内にある変数の集合
-- [SExp] は生成した補助定義のリスト
-- Intはgensymカウンター

defineProgram :: String -> Template -> [SExp]
defineProgram name t =
  case runRWS (encodeTemplate name t) Set.empty (0,[]) of
    (e, _, defs) -> defs ++ [e]

encodeTemplate :: String -> Template -> SMTGen SExp
encodeTemplate name t =
  case t of
    TempNormal x e  -> local (Set.insert x) $ do
      e' <- encodeTExpr name e
      return $ SApply [SAtom "define-fun", SAtom name, SApply [SApply [SAtom x, valueSort]], valueSort, e']
{-
    TempTFold x y e -> do
      e' <- local (Set.union (Set.fromList [x,y])) $ encodeTExpr prefix e
      return $ SApply [ SAtom "lambda", SApply [SAtom x]
                      ,   SApply [SAtom "fold", SAtom x, SAtom "0", SApply [SAtom "lambda", SApply [SAtom x, SAtom y], e']]
                      ]
-}
    TempBonus x e0 e1 e2 -> local (Set.insert x) $ do
      e0' <- encodeTExpr name e0
      e1' <- encodeTExpr name e1
      e2' <- encodeTExpr name e2
      return $ SApply [ SAtom "define-fun", SApply [SAtom x]
                      ,   SApply [SAtom "if0", SApply [SAtom "bvand", e0', one], e1', e2']
                      ]

encodeTExpr :: String -> TExpr -> SMTGen SExp
encodeTExpr prefix = f
  where
    f (TLeaf id vs) = do
      let prefix2 = prefix ++ "-hole" ++ show id ++ "-"
          info = (SAtom (prefix2++"0"), zero, "0") : (SAtom (prefix2++"1"), one, "1") : [(SAtom (prefix2++v), SAtom v, v) | v <- vs]
      forM_ info $ \(sel,_,_) -> do
        tell $ [SApply [SAtom "declare-fun", sel, SApply [], SAtom "Bool"]]
      tell $ [ SApply [ SAtom "assert", atLeastOne [sel | (sel,_,_) <- info] ] ]
      tell $ [ SApply [ SAtom "assert", atMostOne  [sel | (sel,_,_) <- info] ] ]
      saveHoleInfo id [(sel,val) | (sel,_,val) <- info]
      return $ foldr (\(sel,smtval,_) orig -> SApply [SAtom "ite", sel, smtval, orig]) zero info

    f (TIf0 c t e) = do
      c' <- f c
      t' <- f t
      e' <- f e
      return $ SApply [SAtom "ite", SApply [SAtom "=", c', zero], t', e']

    f (TFold e0 e1 y z e2) = do
      fname <- gensym prefix
      fvs <- ask
      let fvs' = fvs `Set.difference` Set.fromList [y,z]
          params = Set.toAscList fvs' ++ [y, z]
      e2' <- local (Set.insert y . Set.insert z) $ f e2
      tell $ [SApply [ SAtom "define-fun", fname, SApply [SApply [SAtom v, valueSort] | v <- params], valueSort, e2' ]]

      e0' <- f e0 -- リスト相当
      e1' <- f e1 -- 初期値
      
      encodeLet prefix e0' $ \tmp -> do
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
        return $ foldr (\a b -> SApply $ fname : [SAtom v | v <- Set.toList fvs'] ++ [a, b]) e1' [a8,a7,a6,a5,a4,a3,a2,a1]

    f (TOp1 id ops e) = do
      e'  <- f e
      encodeLet prefix e' $ \tmp -> do
        let prefix2 = prefix ++ "-hole" ++ show id ++ "-"
            info = [(SAtom (prefix2 ++ render op), SApply [encodeOp1 op, tmp], render op) | op <- ops]
        forM_ info $ \(sel,_,_) -> do
          tell $ [SApply [SAtom "declare-fun", sel, SApply [], SAtom "Bool"]]
        tell $ [ SApply [ SAtom "assert", atLeastOne [sel | (sel,_,_) <- info] ] ]
        tell $ [ SApply [ SAtom "assert", atMostOne  [sel | (sel,_,_) <- info] ] ]
        saveHoleInfo id [(sel,val) | (sel,_,val) <- info]
        return $ foldr (\(sel,smtval,_) orig -> SApply [SAtom "ite", sel, smtval, orig]) zero info

    f (TOp2 id ops e1 e2) = do
      e1' <- f e1
      e2' <- f e2
      encodeLet prefix e1' $ \tmp1 -> do
        encodeLet prefix e2' $ \tmp2 -> do
          let prefix2 = prefix ++ "-hole" ++ show id ++ "-"
              info = [(SAtom (prefix2 ++ render op), SApply [encodeOp2 op, tmp1, tmp2], render op) | op <- ops]
          forM_ info $ \(sel,_,_) -> do
            tell $ [SApply [SAtom "declare-fun", sel, SApply [], SAtom "Bool"]]
          tell $ [ SApply [ SAtom "assert", atLeastOne [sel | (sel,_,_) <- info] ] ]
          tell $ [ SApply [ SAtom "assert", atMostOne  [sel | (sel,_,_) <- info] ] ]
          saveHoleInfo id [(sel,val) | (sel,_,val) <- info]
          return $ foldr (\(sel,smtval,_) orig -> SApply [SAtom "ite", sel, smtval, orig]) zero info

gensym :: String -> SMTGen SExp
gensym prefix = do
  (i,tbl) <- get
  put (i+1, tbl)
  return $ SAtom $ prefix ++ "-" ++ show i

saveHoleInfo :: HoleID -> [(SExp, String)] -> SMTGen SExp
saveHoleInfo = undefined

encodeLet :: String -> SExp -> (SExp -> SMTGen SExp) -> SMTGen SExp
encodeLet prefix e f = do
  tmp <- gensym prefix
  body <- f tmp
  return $ SApply [ SAtom "let", SApply [SApply [tmp, e]], body ]

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
  [ sexp "(define-fun if0 ((c (_ BitVec 64)) (t (_ BitVec 64)) (e (_ BitVec 64))) (_ BitVec 64) (ite (= c #b0000000000000000000000000000000000000000000000000000000000000000) t e))"
  , sexp "(define-fun shl1 ((x (_ BitVec 64))) (_ BitVec 64) (concat ((_ extract 62 0) x) #b0))"
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

atLeastOne :: [SExp] -> SExp
atLeastOne xs = SApply (SAtom "or" : xs)

atMostOne :: [SExp] -> SExp
atMostOne xs = SApply (SAtom "and " : cs)
  where
    cs = do
      x <- xs
      return $ SApply [ SAtom "=>"
                      , x
                      , SApply [SAtom "not", SApply (SAtom "or" : [y | y<-xs, x/=y])]
                      ]

-- ---------------------------------------------------------------------------------------------

type Lab = State Int

labelTemplate' :: Template -> Template
labelTemplate' t = evalState (labelTemplate t) 0

labelTemplate :: Template -> Lab Template
labelTemplate (TempNormal x e) = do
  e' <- labelTExpr e
  return $ TempNormal x e'
labelTFold (TempTFold x y e) = do
  e' <- labelTExpr e
  return $ TempTFold x y e'
labelTFold (TempBonus x e0 e1 e2) = do
  e0' <- labelTExpr e0
  e1' <- labelTExpr e1
  e2' <- labelTExpr e2
  return $ TempBonus x e0' e1' e2'

labelTExpr :: TExpr -> Lab TExpr
labelTExpr = f
  where
    gensym = do
      i <- get
      put $! i+1
      return i

    f (TLeaf _ fvs) = do
      i <- gensym
      return $ TLeaf i fvs
    f (TIf0 e0 e1 e2) = do
      e0' <- f e0
      e1' <- f e1
      e2' <- f e2
      return $ TIf0 e0' e1' e2'
    f (TFold e0 e1 x y e2) = do 
      e0' <- f e0
      e1' <- f e1
      e2' <- f e2
      return $ TFold e0' e1' x y e2'
    f (TOp1 _ xs e0) = do
      i <- gensym
      e0' <- f e0
      return $ TOp1 i xs e0'
    f (TOp2 _ xs e0 e1) = do
      i <- gensym
      e0' <- f e0
      e1' <- f e1
      return $ TOp2 i xs e0' e1'

-- ---------------------------------------------------------------------------------------------


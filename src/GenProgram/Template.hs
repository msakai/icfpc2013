module GenProgram.Template
  (
  -- * Template
    Template
  , Hole
  , holes
  , isGround
  , toProgram

  -- * empty templates
  , empty
  , emptyTFold

  -- * refinement
  , allOps
  , refine
  , refineH
  ) where

import Control.Monad
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import BV

data Template
  = Template
  { tProgram    :: Program
  , holesInfo   :: Map ID HoleInfo
  , unusedVars  :: [ID]
  , unusedHoles :: [Hole]
  }
  deriving (Show)

-- 後から式を埋められる箇所を「穴」と呼ぶ
-- 穴は特別な変数で表現
type Hole = ID

-- 穴から参照できる変数の集合
type HoleInfo = Set ID

holes :: Template -> [Hole]
holes = Map.keys . holesInfo

allVars :: [ID]
allVars = ["x" ++ show i | i <- [(1::Int)..]]

allHoles :: [Hole]
allHoles = ["hole" ++ show i | i <- [(1::Int)..]]

isGround :: Template -> Bool
isGround t = Map.null (holesInfo t)

toProgram :: Template -> Maybe Program
toProgram t
  | isGround t = return $ tProgram t
  | otherwise  = mzero

-- そのテンプレートから生成されうるプログラムの最小のサイズ
instance Measureable Template where
  size Template{ tProgram = p } = size p

-- 一番抽象的なテンプレート: (lambda (x) □1)
empty :: Template
empty
  = Template
  { tProgram     = Program v (Var h)
  , holesInfo   = Map.singleton h (Set.singleton v)
  , unusedVars  = vs
  , unusedHoles = hs
  }
  where
    (v:vs) = allVars
    (h:hs) = allHoles

-- tfoldの場合の一番抽象的なテンプレート:  (lambda (x) (fold x 0 (lambda (x y) □1)))
emptyTFold :: Template
emptyTFold
  = Template
  { tProgram    = Program v1 $ Fold (Var v1) (Const Zero) v2 v3 (Var h)
  , holesInfo   = Map.singleton h (Set.fromList [v2,v3]) -- v1を含まないことに注意
  , unusedVars  = vs
  , unusedHoles = hs
  }
  where
    (v1:v2:v3:vs) = allVars
    (h:hs) = allHoles

allOps :: [String]
allOps =
  [render o | o <- [(minBound::Op1)..maxBound]] ++
  [render o | o <- [(minBound::Op2)..maxBound]] ++
  ["if0", "fold"]

refine :: Template -> [String] -> [Template]
refine t ops =
  case holes t of
    h:_ -> refineH t h ops
    []  -> []

refineH :: Template -> Hole -> [String] -> [Template]
refineH t@Template{ tProgram = prog } h ops =
  msum
  [ do b <- [Zero, One]
       let e = Const b
       return $ t'{ tProgram = fill prog h e }
  , do v <- Set.toList fvs
       let e = Var v
       return $ t'{ tProgram = fill prog h e }
  , do guard $ "if0" `elem` ops
       let (hc:ht:he:hs) = unusedHoles t
           e = If0 (Var hc) (Var ht) (Var he)
       return $ t'{ tProgram    = fill prog h e
                  , holesInfo   = Map.fromList [(hc,fvs),(ht,fvs),(he,fvs)] `Map.union` holesInfo t'
                  , unusedHoles = hs
                  }
  , do guard $ "fold" `elem` ops
       let (y:z:vs)      = unusedVars t
           (h0:h1:h2:hs) = unusedHoles t
           fvs'          = Set.fromList [y,z] `Set.union` fvs
           e             = Fold (Var h0) (Var h1) y z (Var h2)
       return $ t'{ tProgram    = fill prog h e
                  , holesInfo   = Map.fromList [(h0,fvs),(h1,fvs),(h2,fvs')] `Map.union` holesInfo t'
                  , unusedVars  = vs
                  , unusedHoles = hs
                  }
  , do op <- [minBound..maxBound]
       guard $ render op `elem` ops
       let (h1:hs) = unusedHoles t
           e = Op1 op (Var h1)
       return $ t'{ tProgram    = fill prog h e
                  , holesInfo   = Map.insert h1 fvs (holesInfo t')
                  , unusedHoles = hs
                  }
  , do op <- [minBound..maxBound]
       guard $ render op `elem` ops
       let (h1:h2:hs) = unusedHoles t
           e = Op2 op (Var h1) (Var h2)
       return $ t'{ tProgram    = fill prog h e
                  , holesInfo   = Map.insert h1 fvs $ Map.insert h2 fvs $ holesInfo t'
                  , unusedHoles = hs
                  }
  ]
  where
    fvs = holesInfo t Map.! h -- free variables
    t' = t{ holesInfo = Map.delete h (holesInfo t) } 

fill :: Program -> Hole -> Expr -> Program
fill (Program x e1) h e2 = Program x (f e1)
  where
    f (Const b) = Const b
    f v@(Var id)
      | h == id   = e2
      | otherwise = v
    f (If0 c t e) = If0 (f c) (f t) (f e)
    f (Fold e0 e1 y z e2) = Fold (f e0) (f e1) y z (f e2)
    f (Op1 op e)     = Op1 op (f e)
    f (Op2 op e1 e2) = Op2 op (f e1) (f e2)

module GenProgram.TemplateBased
  ( ProgramSet
  , generate
  , pickup'
  , filterByExamples
  , numTemplates

  -- * low-level
  , psTemplates
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)

import BV
import GenProgram.Template
import qualified GenProgram.DP as DP
import qualified SMT

data ProgramSet
  = ProgramSet
  { psTemplates :: [Template]
  , psBlocked   :: Set Program
  , psSize      :: !Int
  , psOps       :: [String]
  , psExamples  :: Map Value Value
  }

generate :: [String] -> Int -> ProgramSet
generate ops size =
  ProgramSet
  { psTemplates = [if "tfold" `elem` ops then emptyTFold else empty]
  , psBlocked   = Set.empty
  , psSize      = size
  , psOps       = ops
  , psExamples  = Map.empty
  }

pickup :: ProgramSet -> Maybe Program
pickup ps = listToMaybe [p | p <- evalState (runListT m) Map.empty, p `Set.notMember` psBlocked ps]
  where
    m = msum [generateProgram' t (psOps ps) (psSize ps) | t <- psTemplates ps]

pickup' :: ProgramSet -> Maybe (Program, ProgramSet)
pickup' ps =
  case DP.runGen (f (psTemplates ps)) of
    Nothing -> Nothing
    Just (prog, ts') -> Just (prog, ps{ psTemplates = ts', psBlocked = Set.insert prog (psBlocked ps) })
  where
    f :: [Template] -> DP.Gen (Maybe (Program, [Template]))
    f [] = return $ Nothing
    f tts@(t:ts) = do
      progs <- runListT $ generateProgram' t (psOps ps) (psSize ps)
      case filter (`Set.notMember` psBlocked ps) progs of
        [] -> f ts
        (prog:_) -> return $ Just (prog, ts ++ refine t (psOps ps))

filterByExamples :: ProgramSet -> [(Value,Value)] -> ProgramSet
filterByExamples ps@ProgramSet{ psTemplates = ts1, psExamples = ios1 } ios' =
  ps{ psTemplates = ts2, psExamples = ios2 }
  where
    ts2  = filter (\t -> SMT.checkTemplateSAT t (Map.toList ios2)) ts1
    ios2 = Map.fromList ios' `Map.union` ios1

numTemplates :: ProgramSet -> Int
numTemplates ps = length (psTemplates ps)

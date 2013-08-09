{-# LANGUAGE
    OverloadedStrings
 #-}

module Interaction where

import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative ((<$>),(<*>),empty)
import Data.Aeson
import Data.Either
import Data.Maybe
import Network.HTTP
import Network.Stream as N

import Prelude as P

data Problem = Problem
  { probId        :: String
  , probSize      :: Int
  , probOperators :: [String]
  , probSolved    :: Maybe Bool
  , probTimeLeft  :: Maybe Bool
  } deriving (Show)

instance FromJSON Problem where
  parseJSON (Object v) = Problem
                       <$> v .:  "id"
                       <*> v .:  "size"
                       <*> v .:  "operators"
                       <*> v .:? "solved"
                       <*> v .:? "timeLeft"

instance ToJSON Problem where
  toJSON v = object $ catMaybes [Just $ ("id"        .=)  $  probId v
                                ,Just $ ("size"      .=)  $  probSize v
                                ,Just $ ("operators" .=)  $  probOperators v
                                ,       ("solved"    .=) <$> probSolved v
                                ,       ("timeLeft"  .=) <$> probTimeLeft v
                                ]

sampleProblem :: BL.ByteString
sampleProblem = "{\"id\":\"dKdeIAoZMyb5y3a74iTcLXyr\",\"size\":30,\"operators\":[\"shr16\",\"if0\",\"xor\",\"plus\",\"not\",\"fold\"]}"

sample2 = encode $ Problem "dKdeIAoZMyb5y3a74iTcLXyr" 30 ["shr16","if0","xor","plus","not","fold"] Nothing Nothing

token :: String
token = "0200tHPjE5fbtiDWDxwEBpuJgTzZaD5pTQPTrABZ"++"vpsH1H"

url :: String -> String
url path = "http://icfpc2013.cloudapp.net/"++path++"?auth="++token

rqMyproblems :: Request_String
rqMyproblems = postRequest (url "myproblems")

rqEvalprog   :: Either ProgId Prog -> Args -> Request_String
rqEvalprog prog args  = postRequestWithBody (url "eval") "application/json" 
                      (mkEvalRequestBody prog args)
rqSubmitguess :: ProbId -> Prog -> Request_String
rqSubmitguess prob prog = postRequestWithBody (url "guess") "application/json"
                        (mkGuessRequestBody prob prog)

rqTraining :: Maybe Size -> Maybe [Operator] -> Request_String
rqTraining Nothing Nothing = postRequest (url "train")
rqTraining size ops = postRequestWithBody (url "train") "application/json"
                      (mkTrainRequestBody size ops)

rqStatus :: Request_String
rqStatus = postRequest (url "status")

mkEvalRequestBody :: Either ProgId Prog -> Args -> String
mkEvalRequestBody (Left pid) args
  = BL.unpack $ encode 
  $ object ["id" .= pid ,"aruguments" .= args ]
mkEvalRequestBody (Right prog) args
  = BL.unpack $ encode 
  $ object ["program" .= prog ,"aruguments" .= args ]

mkGuessRequestBody :: ProbId -> Prog -> String
mkGuessRequestBody prob prog
  = BL.unpack $ encode 
  $ object ["id" .= prob, "prog" .= prog]

mkTrainRequestBody :: Maybe Size -> Maybe [Operator] -> String
mkTrainRequestBody (Just sz) Nothing 
  = BL.unpack $ encode 
  $ object ["size" .= sz]
mkTrainRequestBody Nothing (Just ops)
  = BL.unpack $ encode 
  $ object ["operators" .= ops]
mkTrainRequestBody (Just sz) (Just ops)
  = BL.unpack $ encode 
  $ object ["size" .= sz, "operators" .= ops]

type ProbId   = String
type ProgId   = String
type Prog     = String
type Args     = [String]

type Size     = Int
type Operator = String

type Problems        = [Value]
type EvalResponse    = String
type GuessResponse   = String
type TrainingProblem = String
type Status          = String

getProblems :: IO (Maybe Value)
getProblems = (simpleHTTP rqMyproblems :: IO (N.Result (Response String)))
  >>= getResponseBody >>= return . decode . BL.pack

evalProgram :: Either ProgId Prog -> Args -> IO (Maybe Value)
evalProgram prog args = (simpleHTTP (rqEvalprog prog args) :: IO (N.Result (Response String)))
  >>= getResponseBody >>= return . decode . BL.pack
              
submitGuess :: ProbId -> Prog -> IO (Maybe Value)
submitGuess prob prog = (simpleHTTP (rqSubmitguess prob prog) :: IO (N.Result (Response String)))
  >>= getResponseBody >>= return . decode . BL.pack

training :: Maybe Size -> Maybe [Operator] -> IO (Maybe Value)
training size ops = (simpleHTTP (rqTraining size ops) :: IO (N.Result (Response String)))
  >>= getResponseBody >>= return . decode . BL.pack

status :: IO (Maybe Value)
status = (simpleHTTP rqStatus :: IO (N.Result (Response String)))
  >>= getResponseBody >>= return . decode . BL.pack

responseToValue :: N.Result (Response String) -> IO (Maybe Value)
responseToValue res = getResponseBody res >>= return . decode . BL.pack


-- ^ myproblems
myproblems :: IO (Maybe [Problem])
myproblems = do
  str <- readFile "data/myproblems.json"
  let lb = BL.pack str
  return $ decode lb

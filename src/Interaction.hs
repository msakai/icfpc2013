{-# LANGUAGE
    OverloadedStrings
 #-}

module Interaction where

import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative ((<$>),(<*>))
import Data.Aeson
import Data.Aeson.Types as A
import Data.Either
import Data.Maybe
import Network.HTTP
import Network.Stream as N

import Prelude as P

-- Type synonyms

type ProbId   = String

type ProgId   = String
type Prog     = String
type Arg      = String
type Stat     = String
type Output   = String
type Message  = String

type Size     = Int
type Operator = String

-- Problem

data Problem = Problem
  { probId        :: ProbId
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

-- EvalRequest

data EvalRequest = EvalRequest
  { evrqId        :: Maybe ProgId
  , evrqProgram   :: Maybe Prog
  , evrqArguments :: [Arg]
  } deriving (Show)

instance FromJSON EvalRequest where
  parseJSON (Object v) = EvalRequest 
                       <$> v .:? "id"
                       <*> v .:? "program"
                       <*> v .:  "arguments"

instance ToJSON EvalRequest where
  toJSON v = object $ catMaybes [       ("id"      .=)   <$> evrqId v
                                ,       ("program" .=)   <$> evrqProgram v
                                ,Just $ ("arguments" .=)  $  evrqArguments v
                                ]

-- EvalResponse

data EvalResponse = EvalResponse
  { evrsStatus  :: Stat
  , evrsOutputs :: Maybe [Output]
  , evrsMessage :: Maybe Message
  }

instance FromJSON EvalResponse where
  parseJSON (Object v) = EvalResponse
                       <$> v .:  "status"
                       <*> v .:? "outputs"
                       <*> v .:? "message"

instance ToJSON EvalResponse where
  toJSON v = object $ catMaybes [Just $ ("status"  .=)  $  evrsStatus v
                                ,       ("outputs" .=) <$> evrsOutputs v
                                ,       ("message" .=) <$> evrsMessage v
                                ]

-- GuessRequest

data GuessRequest = GuessRequest
  { gsrqId        :: ProgId
  , gsrqProgram   :: Prog
  } deriving (Show)

instance FromJSON GuessRequest where
  parseJSON (Object v) = GuessRequest 
                       <$> v .: "id"
                       <*> v .: "program"

instance ToJSON GuessRequest where
  toJSON v = object $ catMaybes [Just $ ("id"      .=)   $ gsrqId v
                                ,Just $ ("program" .=)   $ gsrqProgram v
                                ]

-- GuessResponse

data GuessResponse = GuessResponse
  { gsrsStatus    :: Stat
  , gsrsValues    :: Maybe [String]
  , gsrsMessage   :: Maybe Message
  , gsrsLightning :: Maybe Bool
  }
  deriving (Show)

instance FromJSON GuessResponse where
  parseJSON (Object v) = GuessResponse
                       <$> v .:  "status"
                       <*> v .:? "values"
                       <*> v .:? "message"
                       <*> v .:? "lightning"

instance ToJSON GuessResponse where
  toJSON v = object $ catMaybes [Just $ ("status"    .=)  $  gsrsStatus v
                                ,       ("valuess"   .=) <$> gsrsValues v
                                ,       ("message"   .=) <$> gsrsMessage v
                                ,       ("lightning" .=) <$> gsrsLightning v
                                ]

-- TrainRequest

data TrainRequest = TrainRequest
  { trrqSize      :: Maybe Size
  , trrqOperators :: Maybe [Operator]
  } deriving (Show)

instance FromJSON TrainRequest where
  parseJSON (Object v) = TrainRequest 
                       <$> v .:? "size"
                       <*> v .:? "operators"

instance ToJSON TrainRequest where
  toJSON v = object $ catMaybes [("size"      .=) <$> trrqSize v
                                ,("operators" .=) <$> trrqOperators v
                                ]

-- TrainingProblem

data TrainingProblem = TrainingProblem
  { trprChallenge :: Prog
  , trprId        :: ProbId
  , trprSize      :: Size
  , trprOperators :: [Operator]
  }

instance FromJSON TrainingProblem where
  parseJSON (Object v) = TrainingProblem
                       <$> v .:  "challenge"
                       <*> v .:  "id"
                       <*> v .:  "size"
                       <*> v .:  "operators"

instance ToJSON TrainingProblem where
  toJSON v = object $ catMaybes [Just $ ("challenge" .=)  $  trprChallenge v
                                ,Just $ ("id"        .=)  $  trprId v
                                ,Just $ ("size"      .=)  $  trprSize v
                                ,Just $ ("operators" .=)  $  trprOperators v
                                ]

-- Status

data Status = Status
  { stEasyChairId    :: String
  , stConntestScore  :: Int
  , stLightningScore :: Int
  , stTrainingScore  :: Int
  , stMismatches     :: Int
  , stNumRequests    :: Int
  , stRequestWindow  :: RequestWindow
  , stCpuWindow      :: CpuWindow
  , stCpuTotalTime   :: Int
  } deriving (Show)

data RequestWindow = RequestWindow
  { rqwResetsIn :: Int
  , rqwAmount   :: Int
  , rqwLimit    :: Int
  } deriving (Show)

data CpuWindow = CpuWindow
  { cpwResetsIn :: Int
  , cpwAmount   :: Int
  , cpwLimit    :: Int
  } deriving (Show)

instance FromJSON Status where
  parseJSON (Object v) = Status
                       <$> v .: "easyChairId"
                       <*> v .: "contestScore"
                       <*> v .: "lightningScore"
                       <*> v .: "trainingScore"
                       <*> v .: "mismatches"
                       <*> v .: "numRequests"
                       <*> v .: "requestWindow"
                       <*> v .: "cpuWindow"
                       <*> v .: "cpuTotalTime"

instance FromJSON RequestWindow where
  parseJSON (Object v) = RequestWindow
                       <$> v .: "resetsIn"
                       <*> v .: "amount"
                       <*> v .: "limit"

instance FromJSON CpuWindow where
  parseJSON (Object v) = CpuWindow
                       <$> v .: "resetsIn"
                       <*> v .: "amount"
                       <*> v .: "limit"

instance ToJSON Status where
  toJSON v = object [ "easyChairId"     .= stEasyChairId v
                    , "conntestScore"   .= stConntestScore v
                    , "lightningScore"  .= stLightningScore v
                    , "trainingScore"   .= stTrainingScore v
                    , "mismatches"      .= stMismatches v
                    , "numRequests"     .= stNumRequests v
                    , "requestWindow"   .= stRequestWindow v
                    , "cpuWindow"       .= stCpuWindow v
                    , "cpuTotalTime"    .= stCpuTotalTime v
                    ]

instance ToJSON RequestWindow where
  toJSON v = object [ "resetsIn"  .= rqwResetsIn v
                    , "amount"    .= rqwAmount v
                    , "limit"     .= rqwLimit v
                    ]

instance ToJSON CpuWindow where
  toJSON v = object [ "resetsIn"  .= cpwResetsIn v
                    , "amount"    .= cpwAmount v
                    , "limit"     .= cpwLimit v
                    ]

-- Token

token :: String
token = "0200tHPjE5fbtiDWDxwEBpuJgTzZaD5pTQPTrABZ"++"vpsH1H"

-- Request URI

url :: String -> String
url path = "http://icfpc2013.cloudapp.net/"++path++"?auth="++token

-- Request Message

rqMyproblems :: Request_String
rqMyproblems = postRequest (url "myproblems")

rqEvalprog   :: Either ProgId Prog -> [Arg] -> Request_String
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

-- Request Body

mkEvalRequestBody :: Either ProgId Prog -> [Arg] -> String
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

-- get response by WebAPI

getProblemsStr :: IO (N.Result (Response String))
getProblemsStr = simpleHTTP rqMyproblems

evalProgramStr :: Either ProgId Prog -> [Arg] -> IO (N.Result (Response String))
evalProgramStr prog args = (simpleHTTP (rqEvalprog prog args) :: IO (N.Result (Response String)))
              
submitGuessStr :: ProbId -> Prog -> IO (N.Result (Response String))
submitGuessStr prob prog = simpleHTTP (rqSubmitguess prob prog)

trainingStr :: Maybe Size -> Maybe [Operator] -> IO (N.Result (Response String))
trainingStr size ops = simpleHTTP (rqTraining size ops)

statusStr :: IO (N.Result (Response String))
statusStr = simpleHTTP rqStatus

-- Response to JSON value

responseToValue :: N.Result (Response String) -> IO (Maybe Value)
responseToValue res = getResponseBody res >>= return . decode . BL.pack

-- Getting response data

getProblem :: IO (Maybe (A.Result Problem))
getProblem = getProblemsStr >>= responseToValue >>= return . maybe Nothing (Just . fromJSON)

evalProgram :: Either ProgId Prog -> [Arg] -> IO (Maybe (A.Result EvalResponse))
evalProgram prog args = evalProgramStr prog args >>= responseToValue >>= return . maybe Nothing (Just . fromJSON)

              
submitGuess :: ProbId -> Prog -> IO (Maybe (A.Result GuessResponse))
submitGuess prob prog = submitGuessStr prob prog >>=  responseToValue >>= return . maybe Nothing (Just . fromJSON)

training :: Maybe Size -> Maybe [Operator] -> IO (Maybe (A.Result TrainingProblem))
training size ops =  trainingStr size ops >>= responseToValue >>= return . maybe Nothing (Just . fromJSON)

status :: IO (Maybe (A.Result Status))
status = statusStr >>= responseToValue >>= return . maybe Nothing (Just . fromJSON)

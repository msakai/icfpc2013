module Interaction where

import Network.HTTP
import Network.Stream

token :: String
token = "0200tHPjE5fbtiDWDxwEBpuJgTzZaD5pTQPTrABZ"++"vpsH1H"

url :: String -> String
url path = "http://icfpc2013.cloudapp.net/"++path++"?auth="++token

requests :: [String]
requests = ["myproblems","train","eval","guess"]

myproblems,train,eval,guess :: Request_String
[myproblems,train,eval,guess] = map (postRequest.url) requests

type Id   = String
type Prog = String
type Args = [String]

type Problems        = String
type EvalResponse    = String
type GuessResponse   = String
type TrainingProblem = String
type Status          = String

getProblems :: IO (Result (Response Problems))
getProblems = undefined

evalPrograms :: Either Id Prog -> Args -> IO (Result (Response EvalResponse))
evalPrograms = undefined

submitGuess :: Id -> Prog -> IO (Result (Response GuessResponse))
submitGuess = undefined

training :: Either Int [String] -> IO (Result (Response TrainingProblem))
training = undefined

status :: IO (Request (Response Status))
status = undefined

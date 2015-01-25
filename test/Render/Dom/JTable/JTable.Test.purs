module Render.Dom.JTable.Test where

import Data.Argonaut
import Data.Either
import Data.StrMap

import Test.StrongCheck

import Debug.Trace
import Debug.Foreign

import Render.Dom.JTable
import Render.Dom.JTable.Test.Arb

sampleJson = """
  {
    "userId": 8927524,
    "profile": {
      "name":   "Mary Jane",
      "age":    29,
      "gender": "female"
    },
    "comments": [{
        "id":       "F2372BAC",
        "text":     "I concur.",
        "replyTo":  [9817361, "F8ACD164F"],
        "time":     "2015-02-03"
      }, {
        "id":       "GH732AFC",
        "replyTo":  [9654726, "A44124F"],
        "time":     "2015-03-01"
    }]
  }
""" :: String

decodedSample :: Either String JTree
decodedSample = jsonParser sampleJson >>= decodeJson

checkEq :: JTree -> Boolean
checkEq x = x == x && not (x /= x)

main = do 
  trace "JTree test start"

  trace "eq"
  quickCheck checkEq

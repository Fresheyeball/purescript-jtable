module Render.Dom.JTable.Test where

import Data.Argonaut
import qualified Test.StrongCheck as S 
import Debug.Trace
import Test.Unit

import Render.Dom.JTable

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

-- parseTest = do
--   p <- decodeJson sampleJson
--   assert p ==


main = do 
  trace "json parses generically"
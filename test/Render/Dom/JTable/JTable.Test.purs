module Render.Dom.JTable.Test where

import Data.Argonaut
import Data.Either
import Data.StrMap
import Data.Foldable (foldr)
import Test.Unit
import qualified Test.StrongCheck as SC
import Test.StrongCheck.Perturb
import Test.StrongCheck.Gen
import Debug.Trace
import Debug.Foreign

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

newtype TestStrMap a = TestStrMap (StrMap a)

decodedSample :: Either String JTree
decodedSample = jsonParser sampleJson >>= decodeJson

instance arbJTree :: (SC.Arbitrary a) => SC.Arbitrary (TestStrMap a) where
  arbitrary = TestStrMap <$> do
    n  <- SC.arbitrary
    ks <- SC.arbitrary
    return $ foldr (\k sm -> insert k n sm) empty ks

main = do 
  trace "JTree test start"

  trace "eq"
  SC.quickCheck \a -> a == (a :: StrMap)

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

instance arbStrMap :: (SC.Arbitrary a) => SC.Arbitrary (StrMap a) where
  arbitrary = do
    n  <- SC.arbitrary
    ks <- SC.arbitrary
    return $ foldr (\k sm -> insert k n sm) empty (ks :: [String])

instance arbJTree :: SC.Arbitrary JTree where
  arbitrary = chooseInt 0 2 >>= \n -> case n of 
    0 -> JLeaf <$> SC.arbitrary
    1 -> JList <$> SC.arbitrary
    2 -> JMap  <$> SC.arbitrary

checkEq :: StrMap Number -> Boolean
checkEq sn = sn == sn

main = do 
  trace "JTree test start"

  trace "eq"
  SC.smallCheck checkEq

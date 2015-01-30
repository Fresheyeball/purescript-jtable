module Render.Dom.JTable.Test where

import Data.Argonaut
import Data.Argonaut.JCursor
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

-- ([Tuple (JField userId   (JCursorTop))                                              (8927524),
--   Tuple (JField profile  (JField name   (JCursorTop)))                              (\"Mary Jane\"),
--   Tuple (JField profile  (JField age    (JCursorTop)))                              (29),
--   Tuple (JField profile  (JField gender (JCursorTop)))                              (\"female\"),
--   Tuple (JField comments (JIndex 0      (JField id      (JCursorTop))))             (\"F2372BAC\"),
--   Tuple (JField comments (JIndex 0      (JField text    (JCursorTop))))             (\"I concur.\"),
--   Tuple (JField comments (JIndex 0      (JField replyTo (JIndex 0 (JCursorTop)))))  (9817361),
--   Tuple (JField comments (JIndex 0      (JField replyTo (JIndex 1 (JCursorTop)))))  (\"F8ACD164F\"),
--   Tuple (JField comments (JIndex 0      (JField time    (JCursorTop))))             (\"2015-02-03\"),
--   Tuple (JField comments (JIndex 1      (JField id      (JCursorTop))))             (\"GH732AFC\"),
--   Tuple (JField comments (JIndex 1      (JField replyTo (JIndex 0 (JCursorTop)))))  (9654726),
--   Tuple (JField comments (JIndex 1      (JField replyTo (JIndex 1 (JCursorTop)))))  (\"A44124F\"),
--   Tuple (JField comments (JIndex 1      (JField time    (JCursorTop))))             (\"2015-03-01\")])

-- ([Tuple ([0].userId) (8927524),
--   Tuple ([0].profile.name) ("Mary Jane"),
--   Tuple ([0].profile.age) (29),
--   Tuple ([0].profile.gender) ("female"),
--   Tuple ([0].comments[0].id) ("F2372BAC"),
--   Tuple ([0].comments[0].text) ("I concur."),
--   Tuple ([0].comments[0].replyTo[0]) (9817361),
--   Tuple ([0].comments[0].replyTo[1]) ("F8ACD164F"),
--   Tuple ([0].comments[0].time) ("2015-02-03"),
--   Tuple ([0].comments[1].id) ("GH732AFC"),
--   Tuple ([0].comments[1].replyTo[0]) (9654726),
--   Tuple ([0].comments[1].replyTo[1]) ("A44124F"),
--   Tuple ([0].comments[1].time) ("2015-03-01")])

primmms = toPrims <$> jsonParser sampleJson

checkEq :: forall a. (Eq a) => a -> Boolean
checkEq x = (x == x)
     && not (x /= x)

section = trace <<< (++) "\n" <<< flip (++) "\n"





init = do 
  -- section "wowzers"

  -- print primmms

  print $ "Moov"
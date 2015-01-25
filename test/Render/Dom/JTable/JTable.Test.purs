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

sampleJTree = JMap m
  where  
  m  = empty 
    # insert "userId"     ( JLeaf "8927524"    )
    >>> insert "profile"  ( JMap  profile      )
    >>> insert "comments" ( JList comments     )
  profile = empty 
    #   insert "name"     ( JLeaf "\"Mary Jane\""  ) 
    >>> insert "age"      ( JLeaf "29"         )
    >>> insert "gender"   ( JLeaf "\"female\""     )
  comments = [JMap c0, JMap c1]
  c0 = empty
    #   insert "id"       ( JLeaf "\"F2372BAC\""   )
    >>> insert "text"     ( JLeaf "\"I concur.\""  )
    >>> insert "replyTo"  ( JList [JLeaf "9817361", JLeaf "\"F8ACD164F\""])
    >>> insert "time"     ( JLeaf "\"2015-02-03\"" )
  c1 = empty
    #   insert "id"       ( JLeaf "\"GH732AFC\""   )
    >>> insert "replyTo"  ( JList [JLeaf "9654726", JLeaf "\"A44124F\""])
    >>> insert "time"     ( JLeaf "\"2015-03-01\"" )

jsonEqJTree :: String -> JTree -> Boolean
jsonEqJTree x y = case jsonParser x >>= decodeJson of
  Left  _     -> false
  Right jtree -> jtree == y

checkEq :: JTree -> Boolean
checkEq x = x == x && not (x /= x)

main = do 
  trace "JTree test start"

  trace "eq"
  quickCheck' 10 checkEq

  trace "sampleJson"
  assert $ jsonEqJTree sampleJson sampleJTree

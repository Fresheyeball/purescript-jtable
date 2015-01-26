module Render.Dom.JTable.Test where

import Data.Argonaut
import Data.Argonaut.JCursor
import Data.Either
import Data.StrMap

import Test.StrongCheck

import Debug.Trace
import Debug.Foreign

import Render.Dom.JTable
import Render.Dom.JTable.Types
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
  m = empty
    #   insert "userId"   ( JLeaf (JSNumber 8927524)      )
    >>> insert "profile"  ( JMap  profile                 )
    >>> insert "comments" ( JList comments                )
  profile = empty
    #   insert "name"     ( JLeaf (JSString "Mary Jane")  )
    >>> insert "age"      ( JLeaf (JSNumber 29)           )
    >>> insert "gender"   ( JLeaf (JSString "female")     )
  comments = [JMap c0, JMap c1]
  c0 = empty
    #   insert "id"       ( JLeaf (JSString "F2372BAC")   )
    >>> insert "text"     ( JLeaf (JSString "I concur.")  )
    >>> insert "replyTo"  ( JList [JLeaf (JSNumber 9817361), JLeaf (JSString "F8ACD164F") ])
    >>> insert "time"     ( JLeaf (JSString "2015-02-03") )
  c1 = empty
    #   insert "id"       ( JLeaf (JSString "GH732AFC")   )
    >>> insert "replyTo"  ( JList [JLeaf (JSNumber 9654726), JLeaf (JSString "A44124F")   ])
    >>> insert "time"     ( JLeaf (JSString "2015-03-01") )

allLeafTypesString = """
  {
    "its a me"        : "Mario!",
    "i am "           : 30,
    "years old"       : false,
    "you believe me?" : null
  }
""" :: String

allLeafTypesTree = JMap m
  where
  m = empty
    #   insert "its a me"        ( JLeaf (JSString  "Mario!") )
    >>> insert "i am "           ( JLeaf (JSNumber  30)       )
    >>> insert "years old"       ( JLeaf (JSBoolean false)    )
    >>> insert "you believe me?" ( JLeaf (JSNull    jnull)    )

jsonEqJTree :: String -> JTree -> Boolean
jsonEqJTree x y = case jsonParser x >>= decodeJson of
  Left  _     -> false
  Right jtree -> jtree == y

checkEq :: forall a. (Eq a) => a -> Boolean
checkEq x = (x == x)
     && not (x /= x)

section = trace <<< (++) "\n" <<< flip (++) "\n"

main = do
  section "JSemantic test start"

  -- Left ("Couldn't decode.") 
  case jsonParser sampleJson >>= decodeJson of
    Right x -> print (x :: JCursor)
    x       -> print  x

  trace "eq of JSemantic"
  quickCheck' 10 (checkEq :: JSemantic -> Boolean)

  section "JTree test start"

  trace "eq of JTree"
  quickCheck' 10 (checkEq :: JTree -> Boolean)

  trace "sampleJson matches sampleJTree"
  assert $ jsonEqJTree sampleJson sampleJTree

  trace "all json types parse"
  assert $ jsonEqJTree allLeafTypesString allLeafTypesTree

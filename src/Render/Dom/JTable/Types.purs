module Render.Dom.JTable.Types where

import Data.StrMap
import Data.Argonaut.Core ( JString()
                          , JNumber()
                          , JNull()
                          , JBoolean()
                          , JArray()
                          , JObject() )

data JSemantic = JSString  JString
               | JSNumber  JNumber
               | JSBoolean JBoolean
               | JSNull    JNull

data JTree = JMap (StrMap JTree)
           | JList [JTree]
           | JLeaf JSemantic

(/+) :: forall a. (Show a) => String -> a -> String
(/+) x a = x ++ " (" ++ show a ++ ")"

instance showJSemantic :: Show JSemantic where
  show (JSString  a) = "JSString"  /+ a
  show (JSNumber  a) = "JSNumber"  /+ a
  show (JSBoolean a) = "JSBoolean" /+ a
  show (JSNull    a) = "JSNull"    /+ a

instance eqJSemantic :: Eq JSemantic where
  (==) (JSString  a) (JSString  a') = a == a'
  (==) (JSNumber  a) (JSNumber  a') = a == a'
  (==) (JSBoolean a) (JSBoolean a') = a == a'
  (==) (JSNull    a) (JSNull    a') = a == a'
  (==) _             _              = false
  (/=) x y = not $ x == y

instance showJTree :: Show JTree where
  show (JMap  sm) = "JMap"  /+ sm
  show (JList ts) = "JList" /+ ts
  show (JLeaf s ) = "JLeaf" /+ s

instance eqJTree :: Eq JTree where
  (==) (JMap  sm) (JMap  sm') = sm == sm'
  (==) (JList ts) (JList ts') = ts == ts'
  (==) (JLeaf s ) (JLeaf s' ) = s  == s'
  (==) _          _           = false
  (/=) x y = not $ x == y

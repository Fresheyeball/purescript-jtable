module Render.Dom.JTable where

import Data.Argonaut.Core ( Json()
                          , JString()
                          , JNumber()
                          , JNull()
                          , JBoolean()
                          , JArray()
                          , JObject() )
import Data.Argonaut.Decode (DecodeJson)
import Data.Argonaut.JCursor
import Data.Argonaut
import Data.Either
import Data.Maybe
import Data.StrMap
import Data.Tuple
import Data.Foldable (foldr)
import Data.Array.Unsafe (head)
import Text.Smolder.Markup

type Level = Number

data JSemantic = JSString  JString
               | JSNumber  JNumber
               | JSBoolean JBoolean
               | JSNull    JNull

data JTree = JMap (StrMap JTree)
           | JList [JTree]
           | JLeaf JSemantic

foldJTree :: forall a. (JSemantic -> a -> a) -> a -> JTree -> a
foldJTree f i = go where
  go (JMap  sm) = fold  (\i' _ b  -> foldJTree f i' b) i sm
  go (JList ts) = foldr (\b    i' -> foldJTree f i' b) i ts
  go (JLeaf s ) = f s i

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

type JFold f a = f a -> (a -> f a) -> Json -> f a

parseToJTree :: Json -> JTree
parseToJTree = go

  where

  intoLeaf :: forall a. JFold [] a -> (a -> JSemantic) -> Json -> JTree
  intoLeaf f c = f [] (\x -> [x]) >>> head >>> c >>> JLeaf

  go j | isNull    j = j # foldJsonNull    `intoLeaf` JSNull
       | isBoolean j = j # foldJsonBoolean `intoLeaf` JSBoolean
       | isNumber  j = j # foldJsonNumber  `intoLeaf` JSNumber
       | isString  j = j # foldJsonString  `intoLeaf` JSString
       | isArray   j = j # foldJsonArray  []    ((<$>) go) >>> JList
       | isObject  j = j # foldJsonObject empty ((<$>) go) >>> JMap

instance decodeJTree :: DecodeJson JTree where
  decodeJson = Right <<< parseToJTree

module Render.Dom.JTable where

import Data.Argonaut.Core ( Json() )
import Data.Argonaut.Decode (DecodeJson)
import Data.Argonaut
import Data.Either
import Data.StrMap (empty, fold)
import Data.Tuple
import Data.Foldable (foldr)
import Text.Smolder.Markup

import Render.Dom.JTable.Types

type Level = Number

foldJTree :: forall a. (JSemantic -> a -> a) -> a -> JTree -> a
foldJTree f i = go where
  go (JMap  sm) = fold  (\i' _ b  -> foldJTree f i' b) i sm
  go (JList ts) = foldr (\b    i' -> foldJTree f i' b) i ts
  go (JLeaf s ) = f s i

type JFold f a = f a -> (a -> f a) -> Json -> f a

parseToJTree :: Json -> JTree
parseToJTree = go

  where

  intoLeaf :: forall a. JFold [] a -> (a -> JSemantic) -> Json -> JTree
  intoLeaf f c = f [] (\x -> [x]) >>> Data.Array.Unsafe.head >>> c >>> JLeaf

  go j | isNull    j = j # foldJsonNull    `intoLeaf` JSNull
       | isBoolean j = j # foldJsonBoolean `intoLeaf` JSBoolean
       | isNumber  j = j # foldJsonNumber  `intoLeaf` JSNumber
       | isString  j = j # foldJsonString  `intoLeaf` JSString
       | isArray   j = j # foldJsonArray  []    ((<$>) go) >>> JList
       | isObject  j = j # foldJsonObject empty ((<$>) go) >>> JMap

instance decodeJTree :: DecodeJson JTree where
  decodeJson = Right <<< parseToJTree

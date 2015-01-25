module Render.Dom.JTable where

import Data.Argonaut.Core (Json())
import Data.Argonaut.Decode (DecodeJson)
import Data.Argonaut.JCursor 
import Data.Argonaut
import Data.Either
import Data.Maybe
import Data.StrMap
import Data.Tuple
import Data.Foldable (foldr)
import Text.Smolder.Markup

type Level = Number

data JTree = JMap (StrMap JTree) | JList [JTree] | JLeaf String

data ColumnOrdering = M | CustomOrdering (JCursor -> JCursor -> Ordering)

data TableStyle = TableStyle {
  table   :: Level -> Markup -> Markup,
  cell    :: forall a. a -> Markup -> Markup,
  head    :: JCursor -> Markup -> Markup,
  row     :: Markup -> Markup
}

foldJTree :: forall a. (String -> a -> a) -> a -> JTree -> a
foldJTree f i = go
  where    
    go (JMap  sm) = fold  (\i' _ b  -> foldJTree f i' b) i sm
    go (JList ts) = foldr (\b    i' -> foldJTree f i' b) i ts
    go (JLeaf s ) = f s i

instance showJTree :: Show JTree where
  show (JMap  sm) = "JMap (" ++ show sm ++ ")"
  show (JList ts) = "JList (" ++ show ts ++ ")"
  show (JLeaf s ) = "JLeaf \"" ++ s ++ "\""

instance eqJTree :: Eq JTree where
  (==) (JMap  sm) (JMap  sm') = sm == sm'
  (==) (JList ts) (JList ts') = ts == ts'
  (==) (JLeaf s ) (JLeaf s' ) = s  == s'
  (==) _          _           = false 
  (/=) x y = not $ x == y

foldJsonToJTree :: Json -> JTree
foldJsonToJTree = go
  where
  jLeaf = show >>> JLeaf
  go j | isNull    j = jLeaf j
       | isBoolean j = jLeaf j
       | isNumber  j = jLeaf j
       | isString  j = jLeaf j
       | isArray   j = j # foldJsonArray  []    ((<$>) go) >>> JList
       | isObject  j = j # foldJsonObject empty ((<$>) go) >>> JMap

instance decodeJTree :: DecodeJson JTree where
  decodeJson = Right <<< foldJsonToJTree
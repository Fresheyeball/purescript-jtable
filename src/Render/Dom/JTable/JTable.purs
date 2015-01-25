module Render.Dom.JTable where

import Data.Argonaut.Decode (DecodeJson)
import Data.Argonaut.JCursor 
import Data.Argonaut
import Data.Either
import Data.Maybe
import Data.StrMap
import Data.Tuple
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

foldJsonToJTree :: Data.Argonaut.Core.Json -> JTree
foldJsonToJTree = let
    null    = const (JLeaf "") -- null
    boolean = show >>> JLeaf   -- boolean  
    number  = show >>> JLeaf   -- number 
    string  = JLeaf            -- string   ↙array                            ↙object
  in foldJson null boolean number string ((<$>) foldJsonToJTree >>> JList) ((<$>) foldJsonToJTree >>> JMap)
  -- what gives? http://lpaste.net/119159

instance decodeJTree :: DecodeJson JTree where
  decodeJson = Right <<< foldJsonToJTree
module Render.Dom.JTable where

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
foldJTree f i (JMap sm) = fold (\i' _ v -> foldJTree f i' v) i sm

foldJsonToJTree :: Data.Argonaut.Core.Json -> JTree
foldJsonToJTree j | isNull    j = j # show >>> JLeaf
                  | isBoolean j = j # show >>> JLeaf
                  | isNumber  j = j # show >>> JLeaf
                  | isString  j = j # show >>> JLeaf
                  | isArray   j = j # foldJsonArray  []    ((<$>) foldJsonToJTree) >>> JList
                  | isObject  j = j # foldJsonObject empty ((<$>) foldJsonToJTree) >>> JMap                  


-- foldJsonToJTree = foldJson (const $ JLeaf "") (show >>> JLeaf) (show >>> JLeaf) JLeaf ((<$>) foldJsonToJTree >>> JList) ((<$>) foldJsonToJTree >>> JMap)

instance decodeJTree :: DecodeJson JTree where
  decodeJson = Right <<< foldJsonToJTree
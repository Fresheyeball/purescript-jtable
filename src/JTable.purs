module JTable where

import Data.Argonaut.Core ( Json() )
import Data.Argonaut.Decode ( DecodeJson )
import Data.Argonaut.JCursor
import Data.Argonaut
import Data.Either
import Data.Tuple
import Data.Map
import Data.Foldable (foldr, mconcat)
import Text.Smolder.Markup
import Text.Smolder.HTML (td,tr,th,thead,tbody,table)
import Text.Smolder.Renderer.String (render)

import Debug.Spy (spy)
import JTable.Types 

normalizeCursor :: JCursor -> JCursor
normalizeCursor jc = case jc of

  JCursorTop             -> JCursorTop
  JField f (JIndex _ jc) -> normalizeCursor $ JField f jc
  JField f jc            -> JField f $ normalizeCursor jc
  JIndex _ jc            -> normalizeCursor jc


uniform :: [JsonPrim] -> Uniformity
uniform = fst <<< foldr f (Tuple Homogeneous 0)
  
  where

  f _  (Tuple Heterogeneous x) = Tuple Heterogeneous x
  f jp (Tuple _ 0)             = Tuple Homogeneous $ testPrim jp  
  f jp (Tuple _ i)             = Tuple     
    (if i == i' then Homogeneous else Heterogeneous) i'
    where i' = testPrim jp  

  testPrim = testPrim' <<< primToJson 

    where

    testPrim' jp | isNull    jp = 1
                 | isString  jp = 2
                 | isBoolean jp = 3
                 | isNumber  jp = 4  

renderJTable _ = td $ text "foo"

parseNRender :: String -> Either String Markup
parseNRender x = renderJTable <$> jsonParser x


collect :: forall a. JCursor -> a -> (a -> a) -> Map JCursor a -> Map JCursor a
collect njc pu f tdm = if member njc            tdm
                       then alter ((<$>) f) njc tdm
                       else insert njc pu       tdm       

sortToMaps :: [Tuple JCursor JsonPrim] -> Tuple THMap TDMap
sortToMaps = foldr f emptyZipper

  where 

  emptyZipper = Tuple ( empty :: THMap ) ( empty :: TDMap )

  f :: Tuple JCursor JsonPrim -> Tuple THMap TDMap -> Tuple THMap TDMap
  f (Tuple jc jp) (Tuple thm' tdm') = go jc thm' tdm'

    where 

    collect' = jc # normalizeCursor >>> collect

    go :: JCursor -> THMap -> TDMap -> Tuple THMap TDMap

    go (JField s JCursorTop) thm tdm = let topTD = newTD 0 0 0 jp
      in Tuple thm $ collect' [topTD] (\tds -> tds <> [topTD]) tdm

    -- go ()


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

type Level  = Number
type Index  = Number
type Depth  = Number
type Length = Number 
type Height = Number

data TH = TH { level       :: Level
             , depth       :: Depth
             , length      :: Length
             , uniformity  :: Uniformity }

data TD = TD { level       :: Level
             , index       :: Index             
             , height      :: Height
             , value       :: JsonPrim }

type THMap = Map JCursor  TH
type TDMap = Map JCursor [TD]

newTH :: Level -> Depth -> Length -> Uniformity -> TH 
newTH l d l' u = TH { level : l, depth : d, length : l', uniformity : u }

newTD :: Level -> Index -> Height -> JsonPrim -> TD
newTD l i h v = TD { level : l, index : i, height : h, value : v }

instance showTH :: Show TH where
  show (TH th) = "TH { level : "      <> show th.level
                 <> ", depth : "      <> show th.depth
                 <> ", length : "     <> show th.length
                 <> ", uniformity : " <> show th.uniformity <> " }"

instance showTD :: Show TD where
  show (TD td) = "TD { level : "      <> show td.level
                 <> ", index : "      <> show td.index
                 <> ", height : "     <> show td.height
                 <> ", value : "      <> show td.value <> " }"

type Row    = [[Markup]]

data Uniformity = Heterogeneous | Homogeneous

instance showUniformity :: Show Uniformity where

  show Heterogeneous 
    = "Heterogeneous"
  show Homogeneous 
    = "Homogeneous"

instance eqUniformity :: Eq Uniformity where

  (==) Homogeneous   Homogeneous   = true
  (==) Heterogeneous Heterogeneous = true
  (==) _ _ = false
  (/=) x y = not $ x == y 

normalizeCursor :: JCursor -> JCursor
normalizeCursor jc = case jc of

  JCursorTop             -> JCursorTop
  JField f (JIndex _ jc) -> normalizeCursor $ JField f jc
  JField f jc            -> JField f $ normalizeCursor jc
  JIndex _ jc            -> normalizeCursor jc

collect :: Tuple JCursor TH -> THMap -> THMap
collect (Tuple jc jp) m = insert (normalizeCursor jc) jp m 

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


-- sortToMaps :: [Tuple JCursor JsonPrim] -> Tuple THMap TDMap
-- sortToMaps = foldr f emptyZipper

--   where 

--   emptyZipper = Tuple ( empty :: THMap ) ( empty :: TDMap )

--   f :: Tuple JCursor JsonPrim -> 
--   f jj@(Tuple jc jp) tm@(Tuple _th _td) = do  
--       collect jj 
--       g (Tuple (JField s JCursorTop) jp) (Tuple _th _td) = 

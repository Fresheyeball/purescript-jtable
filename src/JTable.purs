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

type Level = Number
type Row   = [[Markup]]

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

collect :: Tuple JCursor JsonPrim -> Map JCursor [JsonPrim] -> Map JCursor [JsonPrim]
collect (Tuple jc' jp) m = let jc = normalizeCursor jc'
                           in if member jc m                               
                              then alter (\mv -> (<>) [jp] <$> mv) jc m 
                              else insert jc [jp] m 

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

renderJTable :: Json -> Markup
-- renderJTable json = toPrims json # flip foldr empty \(Tuple jc v) m ->
--   let 
--     jc' = normalizeCursor jc
--   in if member jc' m then 
renderJTable _ = td $ text "foo"

parseNRender :: String -> Either String Markup
parseNRender x = renderJTable <$> jsonParser x

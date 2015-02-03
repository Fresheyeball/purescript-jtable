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

normalizeCursor :: JCursor -> JCursor
normalizeCursor jc = case jc of
  JCursorTop             -> JCursorTop
  JField f (JIndex _ jc) -> normalizeCursor $ JField f jc
  JField f jc            -> JField f $ normalizeCursor jc
  JIndex _ jc            -> normalizeCursor jc

parseNRender :: String -> Either String Markup
parseNRender x = renderJTable <$> jsonParser x

collect :: Tuple JCursor JsonPrim -> Map JCursor [JsonPrim] -> Map JCursor [JsonPrim]
collect (Tuple jc' jp) m = let jc = normalizeCursor jc'
                           in if member jc m 
                              then alter (\mv -> (<>) [jp] <$> mv) jc m 
                              else insert jc [jp] m 

-- renderJTable :: Json -> Markup
-- renderJTable json = toPrims json # flip foldr empty \(Tuple jc v) m ->
--   let 
--     jc' = normalizeCursor jc
--   in if member jc' m then 

renderJTable _ = td $ text "foo"
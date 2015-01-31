module Render.Dom.JTable where

import Data.Argonaut.Core ( Json() )
import Data.Argonaut.Decode ( DecodeJson )
import Data.Argonaut.JCursor 
import Data.Argonaut
import Data.Either
import Data.StrMap (empty, fold)
import Data.Tuple
import Data.Foldable (foldr)
import Text.Smolder.Markup 
import Text.Smolder.HTML (div,td,tr)
import Text.Smolder.Renderer.String (render)

type Level = Number

build :: Number 
      -> Tuple 
         JCursor 
         JsonPrim
      -> Tuple 
         [[Markup]] -- <thead>
         [[Markup]] -- <tbody>
      -> Tuple
         [[Markup]] -- <thead>
         [[Markup]] -- <tbody>

build i (Tuple JCursorTop p) (Tuple h b@((cells):rows)) 

  = let c = p # show >>> text >>> td    
    in if i == 0 then Tuple h ([c]:b)
                 else Tuple h ((c:cells):rows)
  
-- build i (Tuple (JField field jc) p) (Tuple h b) = 
  
--   = if i == 0 then Tuple () b


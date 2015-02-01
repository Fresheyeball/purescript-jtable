module Render.Dom.JTable where

import Data.Argonaut.Core ( Json() )
import Data.Argonaut.Decode ( DecodeJson )
import Data.Argonaut.JCursor 
import Data.Argonaut
import Data.Either
import Data.Tuple
import Data.Foldable (foldr, mconcat)
import Text.Smolder.Markup 
import Text.Smolder.HTML (td,tr,th,thead,tbody,table)
import Text.Smolder.Renderer.String (render)

import Debug.Spy (spy)

type Level = Number
type Row   = [[Markup]]

emptyRow :: Row
emptyRow = [[]]

build :: Either String [Tuple JCursor JsonPrim] 
      -> Either String Markup
build e = row <$> foldToRows 0 (Tuple emptyRow emptyRow) <$> e 

  where

  row :: Tuple Row Row -> Markup
  row (Tuple h b) = let
      concatTo xss f = f <<< mconcat $ tr <<< mconcat <$> xss
    in table do
      (spy h) `concatTo` thead
      (spy b) `concatTo` tbody

  foldToRows :: Number 
             -> Tuple Row Row 
             -> [Tuple JCursor JsonPrim] 
             -> Tuple Row Row

  foldToRows i tr [t] = build' i tr t
  foldToRows i tr (t:ts) = foldToRows (i + 1) (foldToRows i tr [t]) ts

  build' :: Number
         -> Tuple Row Row -- Tuple thead tbody
         -> Tuple JCursor JsonPrim
         -> Tuple Row Row -- Tuple thead tbody

  build' i (Tuple h b@((cells):rows)) (Tuple JCursorTop prim)

    = let c = prim # show >>> text >>> td    
      in if i == 0 then Tuple h ([c]:b)
                   else Tuple h ((c:cells):rows)
    
  build' i (Tuple h@((cells):rows) b) (Tuple (JField field jc) p) 

    = let c = field # text >>> th
      in if i == 0 then build' i (Tuple ([c]:h) b)          (Tuple jc p)
                   else build' i (Tuple ((c:cells):rows) b) (Tuple jc p)

  build' i (Tuple h b) (Tuple (JIndex index jc) p)

    = build' i (Tuple h b) (Tuple jc p)
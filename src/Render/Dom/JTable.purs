module Render.Dom.JTable where

import Data.Argonaut.Core ( Json() )
import Data.Argonaut.Decode (DecodeJson)
import Data.Argonaut
import Data.Either
import Data.StrMap (empty, fold)
import Data.Tuple
import Data.Foldable (foldr)
import Text.Smolder.Markup (text)
import Text.Smolder.HTML (div)
import Text.Smolder.Renderer.String (render)

type Level = Number

build [Tuple jc p] jc' html = do  
  div <<< text $ "wowzers"

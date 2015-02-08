module JTable.Types where 

import Text.Smolder.Markup
import Data.Argonaut.JCursor
import Data.Argonaut
import Data.Map

type Level    = Number
type Index    = Number
type Depth    = Number
type Length   = Number 
type Height   = Number
type PrimType = Number

data TH = TH { level       :: Level
             , depth       :: Depth
             , length      :: Length
             , primType    :: PrimType
             , uniformity  :: Uniformity }

data TD = TD { level       :: Level
             , index       :: Index             
             , height      :: Height
             , value       :: JsonPrim }

type THMap = Map JCursor  TH
type TDMap = Map JCursor [TD]

newTH ::                Level ->   Depth ->   Length ->    PrimType ->    Uniformity -> TH 
newTH l d l' pt u = TH {level : l, depth : d, length : l', primType : pt, uniformity : u}

newTD ::                Level ->   Index ->   Height ->   JsonPrim -> TD
newTD l i h v     = TD {level : l, index : i, height : h, value : v}

unwrapTH :: forall a.  (Level ->   Depth ->   Length ->    PrimType ->    Uniformity -> a) -> TH -> a
unwrapTH f (TH         {level = l, depth = d, length = l', primType = pt, uniformity = u}) = f l d l' pt u

unwrapTD :: forall a.  (Level ->   Index ->   Height ->   JsonPrim -> a) -> TD -> a
unwrapTD f (TD         {level = l, index = i, height = h, value = v}) = f l i h v

mapTH f (TH x) = TH (f x)
mapTD f (TD x) = TD (f x)

instance showTH :: Show TH where
  show (TH th) = "TH { level : "      <> show th.level
                 <> ", depth : "      <> show th.depth
                 <> ", length : "     <> show th.length
                 <> ", primType : "   <> show th.primType
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

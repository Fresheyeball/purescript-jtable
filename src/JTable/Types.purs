module JTable.Types where

import Text.Smolder.Markup
import Data.Argonaut.JCursor
import Data.Argonaut
import Data.Map

type Level    = Number
type Index    = Number
type Width    = Number
type Length   = Number
type Height   = Number
type PrimType = Number

data TH = TH { level       :: Level
             , width       :: Width
             , length      :: Length
             , primType    :: PrimType
             , uniformity  :: Uniformity }

data TD = TD { level       :: Level
             , index       :: Index
             , height      :: Height
             , value       :: JsonPrim }

type THMap = Map JCursor  TH
type TDMap = Map JCursor [TD]

newTH ::                Level ->   Width ->   Length ->    PrimType ->    Uniformity -> TH
newTH l w l' pt u = TH {level : l, width : w, length : l', primType : pt, uniformity : u}

newTD ::                Level ->   Index ->   Height ->   JsonPrim -> TD
newTD l i h v     = TD {level : l, index : i, height : h, value : v}

unwrapTH :: forall a.  (Level ->   Width ->   Length ->    PrimType ->    Uniformity -> a) -> TH -> a
unwrapTH f (TH         {level = l, width = w, length = l', primType = pt, uniformity = u}) = f l w l' pt u

unwrapTD :: forall a.  (Level ->   Index ->   Height ->   JsonPrim -> a) -> TD -> a
unwrapTD f (TD         {level = l, index = i, height = h, value = v}) = f l i h v

mapTH f (TH x) = TH (f x)
mapTD f (TD x) = TD (f x)

instance showTH :: Show TH where
  show (TH th) = "TH { level : "      <> show th.level
                 <> ", width : "      <> show th.width
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

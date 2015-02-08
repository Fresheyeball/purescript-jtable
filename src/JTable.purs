module JTable where

import Data.Argonaut.Core ( Json() )
import Data.Argonaut.Decode ( DecodeJson )
import Data.Argonaut.JCursor
import Data.Argonaut
import Data.Either
import Data.Tuple
import Data.Map
import Data.Foldable (foldr, mconcat)
import Data.Array.Unsafe
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

testPrim :: JsonPrim -> PrimType
testPrim = primToJson >>> t

  where

  t jp | isNull    jp = 1
       | isString  jp = 2
       | isBoolean jp = 3
       | isNumber  jp = 4  

uniform :: JsonPrim -> PrimType -> Uniformity -> Uniformity
uniform _ _ Heterogeneous = Heterogeneous
uniform jp pt Homogeneous = if testPrim jp == pt
                            then Homogeneous
                            else Heterogeneous

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
  f (Tuple jc' jp) (Tuple thm' tdm') = go jc' thm' tdm'

    where 

    collect' :: forall a. a -> (a -> a) -> Map JCursor a -> Map JCursor a
    collect' = jc' # normalizeCursor >>> collect

    pureTD = newTD 0 0 0 jp            :: TD 
    pureTH = newTH 0 0 0 0 Homogeneous :: TH 

    primType = testPrim jp

    updateTH :: TH -> TH 
    updateTH (TH t) = TH t      
      { length      = t.length + 1
      , uniformity  = uniform jp t.primType t.uniformity }

    incrementLevel :: forall r. {level :: Level | r} -> {level :: Level | r}
    incrementLevel t = t{ level = t.level + 1 }

    go :: JCursor -> THMap -> TDMap -> Tuple THMap TDMap

    -- bottom cases

    -- JCursor terminating with a Field
    go (JField _ JCursorTop) thm tdm = let

        thm' = collect'  pureTH updateTH thm
        tdm' = collect' [pureTD] (\tds -> tds <> [pureTD]) tdm

      in Tuple thm' tdm'

    -- JCursor terminating with an Array 
    go (JIndex n JCursorTop) thm tdm = let 

        thm' = collect' pureTH updateTH thm
        t    = newTD 0 n 0 jp 
        tdm' = collect' [t] (\tds -> tds <> [t]) tdm 

      in Tuple thm' tdm'

    -- mid cases

    go (JField _ jc) thm tdm = let

        x    = go jc thm tdm      
        thm' = x # fst >>> collect'  pureTH (mapTH incrementLevel)
        tdm' = x # snd >>> collect' [pureTD] \ts -> 
          init ts <> [incrementLevel `mapTD` last ts]
      
      in Tuple thm' tdm'

    go (JIndex n jc) thm tdm = let 

        x = go jc thm tdm
        updateTD t = t{ level = t.level + 1, index = n }
        tdm' = x # snd >>> collect' [pureTD] \ts ->
          init ts <> [updateTD `mapTD` last ts]

      in Tuple thm tdm'












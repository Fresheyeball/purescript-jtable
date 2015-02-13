module JTable where

import Data.Argonaut.Core ( Json() )
import Data.Argonaut.Decode ( DecodeJson )
import Data.Argonaut.JCursor
import Data.Argonaut
import Data.Either
import Data.Tuple
import Data.Map
import Data.Monoid
import Data.Foldable (foldr, mconcat, foldMap, find)
import Data.Array.Unsafe
import Text.Smolder.Markup
import Text.Smolder.HTML (td,tr,th,thead,tbody,table)
import Text.Smolder.HTML.Attributes (colspan)
import Text.Smolder.Renderer.String (render)

import Debug.Spy (spy)
import JTable.Types

mdefault :: forall a. (Monoid a, Eq a) => a -> a -> a
mdefault x y = if y == mempty then x else y

collect :: forall a b. (Ord b) => b -> a -> (a -> a) -> Map b a -> Map b a
collect b a f mba = if member b            mba
                    then alter ((<$>) f) b mba
                    else insert b a        mba

updateWith :: forall a. (a -> Boolean) -> (a -> a) -> a -> (a -> [a] -> [a]) -> [a] -> [a]
updateWith lookingFor ifFound  default placeAt xs' = let 
    f = flip foldr (Tuple [] false) \x (Tuple xs b) -> 
      if lookingFor x then Tuple (ifFound x : xs) true else Tuple (x:xs) b
  in case f xs' of Tuple xs b -> if b then xs else default `placeAt` xs  

normalizeCursor :: JCursor -> JCursor
normalizeCursor jc = case jc of

  JCursorTop             -> JCursorTop
  JField f (JIndex _ jc) -> normalizeCursor $ JField f jc
  JField f jc            -> JField (mdefault "<blank>" f) $ normalizeCursor jc
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

sortToMaps :: [Tuple JCursor JsonPrim] -> Tuple THMap TDMap
sortToMaps = foldr f emptyZipper

  where

  emptyZipper = Tuple ( empty :: THMap ) ( empty :: TDMap )

  f :: Tuple JCursor JsonPrim -> Tuple THMap TDMap -> Tuple THMap TDMap
  f (Tuple jc' jp) (Tuple thm' tdm') = go jc' thm' tdm'

    where

    collect' :: forall a. a -> (a -> a) -> Map JCursor a -> Map JCursor a
    collect' = jc' # normalizeCursor >>> collect

    pureTD = newTD 1 1 1           jp              :: TD
    pureTH = newTH 1 1 1 (testPrim jp) Homogeneous :: TH

    primType = testPrim jp

    updateTH :: TH -> TH
    updateTH (TH t) = TH t
      { length      = t.length + 1
      , uniformity  = uniform jp t.primType t.uniformity }

    incrementLevel :: forall r. {level :: Level | r} -> {level :: Level | r}
    incrementLevel t = t{ level = t.level + 1 }

    go :: JCursor -> THMap -> TDMap -> Tuple THMap TDMap

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

    go (JField _ jc) thm tdm = let

        x    = go jc thm tdm
        thm' = x # fst >>> collect'  pureTH (mapTH incrementLevel)
        tdm' = x # snd >>> collect' [pureTD] \ts ->
          init ts <> [incrementLevel `mapTD` last ts]

      in Tuple thm' tdm'

    go (JIndex n jc) thm tdm = let

        x          = go jc thm tdm
        thm'       = fst x
        updateTD t = t{ level = t.level + 1, index = n }
        tdm'       = x # snd >>> collect' [pureTD] \ts ->
          init ts <> [updateTD `mapTD` last ts]

      in Tuple thm' tdm'

collapseRow :: (Markup -> Markup) -> Row -> Markup
collapseRow t = foldMap $ tr <<< mconcat <<< (<$>) \(Tuple s n) -> t ! colspan (show n) $ text s

type MRow = Map Number [Tuple String Number]

buildHeader :: THMap -> Markup
buildHeader = collapseRow th <<< values <<< snd <<< foldr go a <<< toList

  where

  a = Tuple 0 (empty :: MRow)

  go :: Tuple JCursor TH -> Tuple Level MRow -> Tuple Level MRow 

  go (Tuple (JField s JCursorTop) (TH t)) (Tuple _ tm) = 
    let n = Tuple s t.width in Tuple 1 $ collect 0 [n] ((:) n) tm

  -- go (Tuple (JField s jc) th@(TH t)) tup = case go (Tuple jc th) tup of 
  --   Tuple i tm -> let 
  --       n    = Tuple s t.width
  --       find ((==) s <<< fst) xs
  --     in Tuple (i + 1) $ collect i [n] ((:) n) tm

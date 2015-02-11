module JTable.Test where

import Data.Argonaut
import Data.Argonaut.Core ( Json() )
import Data.Argonaut.JCursor
import Data.Either
import Data.Map
import Data.Tuple
import Data.String.Regex (regex, replace)
import Data.Foldable (foldr)
import Data.Array (nub, sort)

import Test.StrongCheck
import Test.QuickCheck.Tuple

import Control.Apply ((*>))

import Debug.Trace
import Debug.Spy

import Test.JTable.Arb
import JTable.Types

import JTable

sampleJson = """
{
  "userId": 8927524,
  "profile": {
    "name":   "Mary Jane",
    "age":    29,
    "gender": "female"
  },
  "comments": [{
      "id":       "F2372BAC",
      "text":     "I concur.",
      "replyTo":  [9817361, "F8ACD164F"],
      "time":     "2015-02-03"
    }, {
      "id":       "GH732AFC",
      "replyTo":  [9654726, "A44124F"],
      "time":     "2015-03-01"
  }]
}
""" :: String

section = trace <<< (<>) "\n" <<< flip (<>) "\n"

stringCursor :: JCursor -> String
stringCursor JCursorTop    = "JCursorTop"
stringCursor (JField s jc) = "JField " <> s      <> " (" <> stringCursor jc <> ")"
stringCursor (JIndex i jc) = "JIndex " <> show i <> " (" <> stringCursor jc <> ")"

checkNormalizeJCursor :: JCursor -> Result
checkNormalizeJCursor jc =

  check (normalizeCursor jc)

  <?> "Normalize JCursor: " <> stringCursor jc

  where

  check JCursorTop     = true
  check (JField _ jc') = check jc'
  check (JIndex _ _  ) = false

-- TODO: Refactor to be more pure
checkCollect :: [Tuple JCursor TH] -> Result
checkCollect x = xs == xs'

  <?> "Check Collect: " <> show x   <> "\n"
  <>  "xs:            " <> show xs  <> "\n"
  <>  "xs':           " <> show xs' <> "\n"

  where

  xs  = sort <<< keys
      $ foldr (flip (uncurry $ normalizeCursor >>> collect) id) (empty :: THMap) x
  xs' = sort <<< nub
      $ normalizeCursor <<< fst <$> x

checkUniform :: JsonPrim -> JsonPrim -> JsonPrim -> Result
checkUniform jp jp' jp'' = let

  uniform' z (Tuple pt u) = Tuple pt (uniform z pt u)

  p = Tuple (testPrim jp) Homogeneous

  homo      = snd $ foldr uniform' p [ jp, jp,  jp   ]
  hetro     = snd $ foldr uniform' p [ jp, jp', jp'' ]
  arbHetro  = let

    a = testPrim jp
    b = testPrim jp'
    c = testPrim jp''

    in   a /= b   ||   b /= c   ||   c /= a

  in (homo == Homogeneous &&
    (if arbHetro then hetro == Heterogeneous else true))

    <?> "Uniform: " <> show jp   <> "\n"
    <>  "->       " <> show jp'  <> "\n"
    <>  "->       " <> show jp'' <> "\n"

-- checkMarkit :: [[Tuple String Number]] -> Result
-- checkMarkit (ts:tss) =


-- murf = section "murf" *> case jsonParser sampleJson of
--   Right x -> x # toPrims >>> foldr collect empty >>> print

regDefault = {global : true, ignoreCase : false, multiline : true, sticky : false, unicode : false}

matchIt s = replace $ regex s regDefault

dummyTHM :: THMap
dummyTHM = insert (JField "foo" JCursorTop) (newTH 1 1 1 1 Homogeneous)
         $ insert (JField "baz" JCursorTop) (newTH 1 1 1 1 Homogeneous)
         $ empty

prettyPrint :: forall a e. (Show a) => a -> Control.Monad.Eff.Eff
  ( trace  :: Trace
  , err    :: Control.Monad.Eff.Exception.Exception
  , random :: Control.Monad.Eff.Random.Random | e) Unit
prettyPrint = show
  >>> matchIt "fromList \\[" "\n ["
  >>> matchIt ",Tuple" "\n, Tuple"
  >>> matchIt "\\(" "\t ("
  >>> trace

init = do

  section "JTable"

  trace "Normalize JCursor"
  quickCheck checkNormalizeJCursor

  trace "Collect to Map"
  quickCheck checkCollect

  trace "Uniformity"
  quickCheck checkUniform

  section "foobar"

  -- print $ markit [[Tuple "foo" 5, Tuple "bar" 4],[Tuple "baz" 1]]

  dummyTHM # buildHeader >>> markit >>> print
  --prettyPrint $ sortToMaps <<< toPrims <$> jsonParser sampleJson

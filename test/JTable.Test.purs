module JTable.Test where

import Data.Argonaut
import Data.Argonaut.Core ( Json() )
import Data.Argonaut.JCursor
import Data.Either
import Data.Map
import Data.Tuple
import Data.Foldable (foldr)
import Data.Array (nub, sort)

import Test.StrongCheck
import Test.QuickCheck.Tuple

import Debug.Trace
import Debug.Spy

import Test.JTable.Arb
import Control.Apply ((*>))

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
  
  check (normalizeCursor jc) <?> "Normalize JCursor: " <> stringCursor jc
  
  where
  
  check JCursorTop     = true
  check (JField _ jc') = check jc'
  check (JIndex _ _  ) = false

checkCollect :: [Tuple JCursor JsonPrim] -> Result
checkCollect x = xs == xs' 

  <?> "Check Collect: " <> show x   <> "\n"
  <>  "xs:            " <> show xs  <> "\n"
  <>  "xs':           " <> show xs' <> "\n"
  
  where 
  
  xs  = sort <<< keys $ foldr collect empty x
  xs' = sort <<< nub  $ normalizeCursor <<< fst <$> x

checkUniform :: JsonPrim -> JsonPrim -> JsonPrim -> Result
checkUniform jp jp' jp'' = let 
    
    testPrim jp | isNull    (primToJson jp) = 1
                | isString  (primToJson jp) = 2
                | isBoolean (primToJson jp) = 3
                | isNumber  (primToJson jp) = 4  

    homo      = uniform [ jp, jp,  jp   ]
    hetro     = uniform [ jp, jp', jp'' ]
    arbHetro  = testPrim jp   /= testPrim jp' 
             || testPrim jp'  /= testPrim jp'' 
             || testPrim jp'' /= testPrim jp 

  in (if arbHetro 
      then homo == Homogeneous && hetro == Heterogeneous 
      else homo == Homogeneous)

    <?> "Uniform: " <> show jp   <> "\n"
    <>  "->       " <> show jp'  <> "\n"
    <>  "->       " <> show jp'' <> "\n"

murf = section "murf" *> case jsonParser sampleJson of
  Right x -> print <<< foldr collect empty <<< toPrims $ x

init = do

  trace "Normalize JCursor"
  quickCheck checkNormalizeJCursor

  trace "Collect to Map"
  quickCheck checkCollect

  trace "Uniformity"
  quickCheck checkUniform
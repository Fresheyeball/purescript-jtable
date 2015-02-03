module JTable.Test where

import Data.Argonaut
import Data.Argonaut.Core ( Json() )
import Data.Argonaut.JCursor
import Data.Either
import Data.Map
import Data.Tuple

import Test.StrongCheck
import Test.StrongCheck.Gen
import Test.QuickCheck.Tuple

import Debug.Trace
import Debug.Foreign

import Test.JTable.Arb

import JTable

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

checkCollect :: Tuple JCursor JsonPrim -> Result
checkCollect (Tuple jc jp) = (jc == jc) <?> "Check Collect: "-- <> show x
  -- where 
  -- xs = collect >>> keys
  -- xs' = Data.Array.nub $ (normalizeCursor <<< fst) <$> x


init = do

  trace "Normalize JCursor"
  quickCheck checkNormalizeJCursor

  trace "Collect to Map"
  quickCheck checkCollect
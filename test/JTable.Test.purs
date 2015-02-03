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
import Test.StrongCheck.Gen
import Test.QuickCheck.Tuple

import Debug.Trace
import Debug.Spy

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

checkCollect :: [Tuple JCursor JsonPrim] -> Result
checkCollect x = xs == xs' <?> "Check Collect: " <> show x
  where 
  xs  = sort <<< keys   $ foldr collect empty x
  xs' = sort <<< nub    $ normalizeCursor <<< fst <$> x

init = do

  trace "Normalize JCursor"
  quickCheck checkNormalizeJCursor

  trace "Collect to Map"
  quickCheck checkCollect
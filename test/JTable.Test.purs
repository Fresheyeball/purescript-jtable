module JTable.Test where

import Data.Argonaut
import Data.Argonaut.JCursor
import Data.Either
import Data.StrMap

import Test.StrongCheck
import Test.StrongCheck.Gen

import Debug.Trace
import Debug.Foreign

import JTable

section = trace <<< (<>) "\n" <<< flip (<>) "\n"

instance arbJCursor :: Arbitrary JCursor where
  arbitrary =  do i <- chooseInt 0 2
                  r <- if i == 0 then pure JCursorTop
                       else if i == 1 then JField <$> arbitrary <*> arbitrary
                       else JIndex <$> arbitrary <*> arbitrary
                  return r

stringCursor :: JCursor -> String
stringCursor JCursorTop    = "JCursorTop"
stringCursor (JField s jc) = "JField " <> s      <> " (" <> stringCursor jc <> ")"
stringCursor (JIndex i jc) = "JIndex " <> show i <> " (" <> stringCursor jc <> ")"

checkNormalizeJCursor :: JCursor -> Result
checkNormalizeJCursor jc =
  check jc <?> "Normalize JCursor: " <> stringCursor jc
  where
  check JCursorTop     = true
  check (JField _ jc') = check jc'
  check (JIndex _ _)   = false

init = do
  trace "Normalize JCursor"
  quickCheck checkNormalizeJCursor
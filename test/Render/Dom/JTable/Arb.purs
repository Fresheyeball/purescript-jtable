module Render.Dom.JTable.Test.Arb where

import Test.StrongCheck
import Test.StrongCheck.Gen
import Data.Argonaut.Core ( JString()
                          , JNumber()
                          , JNull()
                          , JBoolean()
                          , JArray()
                          , JObject() )
import Data.StrMap
import Data.Foldable (foldr)

import Render.Dom.JTable

upTo :: forall f. (Monad f) => Number -> GenT f Number
upTo = chooseInt 0

foreign import jnull "var jnull = null;" :: JNull

instance arbStrMap :: (Arbitrary a) => Arbitrary (StrMap a) where
  arbitrary = do
    n  <- arbitrary
    ks <- arbitrary
    return $ foldr (\k sm -> insert k n sm) empty (ks :: [String])
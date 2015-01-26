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
import Render.Dom.JTable.Types

upTo :: forall f. (Monad f) => Number -> GenT f Number
upTo = chooseInt 0

foreign import jnull "var jnull = null;" :: JNull

instance arbStrMap :: (Arbitrary a) => Arbitrary (StrMap a) where
  arbitrary = do
    n  <- arbitrary
    ks <- arbitrary
    return $ foldr (\k sm -> insert k n sm) empty (ks :: [String])

instance arbJTree :: Arbitrary JTree where
  arbitrary = chooseInt 0 2 >>= \n -> case n of
    0 -> JLeaf <$> arbitrary
    1 -> JList <$> arbitrary
    2 -> JMap  <$> arbitrary

instance arbJSemantic :: Arbitrary JSemantic where
  arbitrary = chooseInt 0 3 >>= \n -> case n of
    0 -> JSString  <$> arbitrary
    1 -> JSNumber  <$> arbitrary
    2 -> JSBoolean <$> arbitrary
    3 -> return $ JSNull jnull

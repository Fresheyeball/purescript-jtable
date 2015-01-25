module Render.Dom.JTable.Test.Arb where

import Test.StrongCheck
import Test.StrongCheck.Gen
import Data.StrMap
import Data.Foldable (foldr)
import Render.Dom.JTable

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
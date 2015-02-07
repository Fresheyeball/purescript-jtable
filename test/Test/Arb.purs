module Test.JTable.Arb where

import Data.Argonaut
import Data.Argonaut.JCursor
import Data.Argonaut.Core (Json())
import Data.Array
import Data.Tuple
import qualified Data.StrMap as M

import JTable.Types

import Test.StrongCheck
import Test.StrongCheck.Gen

genJNull :: Gen Json
genJNull = pure jsonNull

genJBool :: Gen Json
genJBool = fromBoolean <$> arbitrary

genJNumber :: Gen Json
genJNumber = fromNumber <$> arbitrary

genJString :: Gen Json
genJString = fromString <$> arbitrary

genJArray :: Size -> Gen Json
genJArray sz = fromArray <$> vectorOf sz (genJson $ sz - 1)

genJObject :: Size -> Gen Json
genJObject sz = do
  v <- vectorOf sz (genJson $ sz - 1)
  k <- vectorOf (length v) (arbitrary :: Gen AlphaNumString)
  return $  let f (AlphaNumString s) = s ++ "x"
                k' = f <$> k
            in  fromObject <<< M.fromList <<< nubBy (\a b -> (fst a) == (fst b)) $ zipWith Tuple k' v

genJson :: Size -> Gen Json
genJson 0 = oneOf genJNull [genJBool, genJNumber, genJString]
genJson n = frequency (Tuple 1 genJNull) rest where
  rest = [Tuple 2 genJBool,
          Tuple 2 genJNumber,
          Tuple 3 genJString,
          Tuple 1 (genJArray n),
          Tuple 1 (genJObject n)]

instance arbitraryJson :: Arbitrary Json where
  arbitrary = sized genJson

instance arbitraryJsonPrim :: Arbitrary JsonPrim where
  arbitrary = chooseInt 0 3 >>= \x -> case x of
    0 -> primBool <$> arbitrary
    1 -> primNum  <$> arbitrary    
    2 -> primStr  <$> arbitrary
    3 -> return primNull

instance arbJCursor :: Arbitrary JCursor where
  arbitrary =  do i <- chooseInt 0 2
                  r <- if i == 0 then pure JCursorTop
                       else if i == 1 then JField <$> arbitrary <*> arbitrary
                       else JIndex <$> arbitrary <*> arbitrary
                  return r

instance arbitraryUniformity :: Arbitrary Uniformity where
  arbitrary = chooseInt 0 1 >>= \a -> return $ case a of
    0 -> Heterogeneous
    1 -> Homogeneous

instance arbitraryTH :: Arbitrary TH where
  arbitrary = newTH <$> arbitrary 
                    <*> arbitrary 
                    <*> arbitrary 
                    <*> arbitrary
  
instance arbitraryTD :: Arbitrary TD where
  arbitrary = newTD <$> arbitrary
                    <*> arbitrary 
                    <*> arbitrary 
                    <*> arbitrary
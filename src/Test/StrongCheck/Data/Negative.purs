module Test.StrongCheck.Data.Negative where

import Test.StrongCheck.Arbitrary
import Test.StrongCheck.Gen

newtype Negative = Negative Number

runNegative (Negative n) = n

instance arbNegative :: Arbitrary Negative where
  arbitrary = Negative <$> ((*) (-maxNumber)) <$> uniform

instance coarbNegative :: CoArbitrary Negative where
  coarbitrary (Negative n) = coarbitrary n

module Test.StrongCheck.Data.Positive where

import Test.StrongCheck.Arbitrary
import Test.StrongCheck.Gen

newtype Positive = Positive Number

runPositive (Positive n) = n

instance arbPositive :: Arbitrary Positive where
  arbitrary = Positive <$> ((*) maxNumber) <$> uniform

instance coarbPositive :: CoArbitrary Positive where
  coarbitrary (Positive n) = coarbitrary n

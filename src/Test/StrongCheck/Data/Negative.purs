module Test.StrongCheck.Data.Negative
  ( Negative(..)
  , runNegative
  ) where

import Prelude

import Test.StrongCheck.Arbitrary (class Arbitrary, class Coarbitrary, coarbitrary)
import Test.StrongCheck.Gen (uniform)

newtype Negative = Negative Number

runNegative :: Negative -> Number
runNegative (Negative n) = n

derive instance eqNegative :: Eq Negative
derive instance ordNegative :: Ord Negative

instance arbNegative :: Arbitrary Negative where
  arbitrary = Negative <$> (_ * (-maxNumber)) <$> uniform

instance coarbNegative :: Coarbitrary Negative where
  coarbitrary (Negative n) = coarbitrary n

maxNumber :: Number
maxNumber = 9007199254740992.0

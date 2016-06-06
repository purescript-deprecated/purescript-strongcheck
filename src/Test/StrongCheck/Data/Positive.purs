module Test.StrongCheck.Data.Positive
  ( Positive(..)
  , runPositive
  ) where

import Prelude

import Test.StrongCheck.Arbitrary (class Arbitrary, class Coarbitrary, coarbitrary)
import Test.StrongCheck.Gen (uniform)

newtype Positive = Positive Number

runPositive :: Positive -> Number
runPositive (Positive n) = n

derive instance eqPositive :: Eq Positive
derive instance ordPositive :: Ord Positive

instance arbPositive :: Arbitrary Positive where
  arbitrary = Positive <$> (_ * maxNumber) <$> uniform

instance coarbPositive :: Coarbitrary Positive where
  coarbitrary (Positive n) = coarbitrary n

maxNumber :: Number
maxNumber = 9007199254740992.0

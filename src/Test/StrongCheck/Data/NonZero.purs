module Test.StrongCheck.Data.NonZero
  ( NonZero(..)
  , runNonZero
  ) where

import Prelude

import Test.StrongCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)

newtype NonZero  = NonZero Number

runNonZero :: NonZero -> Number
runNonZero (NonZero n) = n

derive instance eqNonZero :: Eq NonZero
derive instance ordNonZero :: Ord NonZero

instance arbNonZero :: Arbitrary NonZero where
  arbitrary = do
    n <- arbitrary
    b <- arbitrary
    let sign = if b then 1.0 else -1.0
    pure $ NonZero (n * maxNumber * sign)

instance coarbNonZero :: Coarbitrary NonZero where
  coarbitrary (NonZero n) = coarbitrary n

maxNumber :: Number
maxNumber = 9007199254740992.0

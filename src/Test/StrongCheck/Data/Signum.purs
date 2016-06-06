module Test.StrongCheck.Data.Signum where

import Prelude

import Test.StrongCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)

newtype Signum = Signum Int

runSignum :: Signum -> Int
runSignum (Signum n) = n

derive instance eqSignum :: Eq Signum
derive instance ordSignum :: Ord Signum

instance arbSignum :: Arbitrary Signum where
  arbitrary = do
    b <- arbitrary
    pure $ Signum (if b then 1 else -1)

instance coarbSignum :: Coarbitrary Signum where
  coarbitrary (Signum n) = coarbitrary n

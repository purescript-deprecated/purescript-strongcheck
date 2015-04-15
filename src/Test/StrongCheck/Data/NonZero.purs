module Test.StrongCheck.Data.NonZero where

import Test.StrongCheck.Arbitrary

newtype NonZero = NonZero Number

runNonZero (NonZero n) = n

instance arbNonZero :: Arbitrary NonZero where
  arbitrary = do n <- arbitrary
                 b <- arbitrary
                 let sign = if b then 1.0 else -1.0
                 return $ NonZero (n * maxNumber * sign)

instance coarbNonZero :: CoArbitrary NonZero where
  coarbitrary (NonZero n) = coarbitrary n

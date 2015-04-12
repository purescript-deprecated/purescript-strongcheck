module Test.StrongCheck.Data.Signum where

import Test.StrongCheck.Arbitrary

newtype Signum = Signum Number

runSignum (Signum n) = n

instance arbSignum :: Arbitrary Signum where
  arbitrary = do b <- arbitrary
                 return $ Signum (if b then 1 else -1)

instance coarbSignum :: CoArbitrary Signum where
  coarbitrary (Signum n) = coarbitrary n

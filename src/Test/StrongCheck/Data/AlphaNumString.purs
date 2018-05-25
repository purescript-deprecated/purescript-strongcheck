module Test.StrongCheck.Data.AlphaNumString where

import Prelude

import Data.String.CodeUnits as SCU
import Test.StrongCheck.Arbitrary (class Arbitrary, class Coarbitrary, coarbitrary)
import Test.StrongCheck.Gen (arrayOf, oneOf)

newtype AlphaNumString = AlphaNumString String

runAlphaNumString :: AlphaNumString -> String
runAlphaNumString (AlphaNumString s) = s

derive instance eqAlphaNumString :: Eq AlphaNumString
derive instance ordAlphaNumString :: Ord AlphaNumString

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = AlphaNumString <<< SCU.fromCharArray <$> arrayOf anyChar
    where
    rest = SCU.toCharArray "bcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    anyChar = oneOf (pure 'a') (map pure rest)

instance coarbAlphaNumString :: Coarbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s

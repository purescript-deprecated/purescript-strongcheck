module Test.StrongCheck.Data.AlphaNumString where

import Prelude

import Data.Maybe (fromJust)
import Data.String as S
import Data.Traversable (sequence)

import Partial.Unsafe (unsafePartial)

import Test.StrongCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)

newtype AlphaNumString = AlphaNumString String

runAlphaNumString :: AlphaNumString -> String
runAlphaNumString (AlphaNumString s) = s

derive instance eqAlphaNumString :: Eq AlphaNumString
derive instance ordAlphaNumString :: Ord AlphaNumString

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = unsafePartial do
    arrNum <- arbitrary
    pure $ fromJust $ (AlphaNumString <<< S.fromCharArray) <$> sequence (lookup <$> arrNum)
    where
    chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    lookup x = S.charAt index chars
      where
      index =
        let
          i1 = S.length chars - 1
          i2 = x * S.length chars
        in if i1 < i2 then i1 else i2

instance coarbAlphaNumString :: Coarbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s

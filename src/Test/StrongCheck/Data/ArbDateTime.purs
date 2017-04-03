module Test.StrongCheck.Data.ArbDateTime where

import Prelude

import Data.DateTime as DT
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)

import Test.StrongCheck.Arbitrary (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Test.StrongCheck.Gen (chooseInt)

newtype ArbTime = ArbTime DT.Time

runArbTime ∷ ArbTime → DT.Time
runArbTime (ArbTime x) = x

derive instance newtypeArbTime ∷ Newtype ArbTime _

instance arbitraryArbTime ∷ Arbitrary ArbTime where
  arbitrary = do
    hour ← chooseInt 0 23
    minute ← chooseInt 0 59
    second ← chooseInt 0 59
    millisecond ← chooseInt 0 999
    pure $ ArbTime $ DT.Time
      (fromMaybe bottom (toEnum hour))
      (fromMaybe bottom (toEnum minute))
      (fromMaybe bottom (toEnum second))
      (fromMaybe bottom (toEnum millisecond))

instance coarbitraryArbTime ∷ Coarbitrary ArbTime where
  coarbitrary (ArbTime t) = do
    _ <- coarbitrary $ fromEnum (DT.hour t)
    _ <- coarbitrary $ fromEnum (DT.minute t)
    _ <- coarbitrary $ fromEnum (DT.second t)
    coarbitrary $ fromEnum (DT.millisecond t)

newtype ArbDate = ArbDate DT.Date

runArbDate ∷ ArbDate → DT.Date
runArbDate (ArbDate x) = x

derive instance newtypeArbDate ∷ Newtype ArbDate _

instance arbitraryArbDate ∷ Arbitrary ArbDate where
  arbitrary = do
    year ← chooseInt 1950 2050
    month ← chooseInt 1 12
    day ← chooseInt 1 31
    pure $ ArbDate $ DT.canonicalDate
      (fromMaybe bottom (toEnum year))
      (fromMaybe bottom (toEnum month))
      (fromMaybe bottom (toEnum day))

instance coarbitraryArbDate ∷ Coarbitrary ArbDate where
  coarbitrary (ArbDate dt) = do
    _ <- coarbitrary $ fromEnum (DT.year dt)
    _ <- coarbitrary $ fromEnum (DT.month dt)
    coarbitrary $ fromEnum (DT.day dt)

newtype ArbDateTime = ArbDateTime DT.DateTime

runArbDateTime ∷ ArbDateTime → DT.DateTime
runArbDateTime (ArbDateTime x) = x

derive instance newtypeArbDateTime ∷ Newtype ArbDateTime _

instance arbitraryArbDateTime ∷ Arbitrary ArbDateTime where
  arbitrary = do
    date <- runArbDate <$> arbitrary
    time <- runArbTime <$> arbitrary
    pure $ ArbDateTime $ DT.DateTime date time

instance coarbitraryArbDateTime ∷ Coarbitrary ArbDateTime where
  coarbitrary (ArbDateTime dt) = do
    _ <- coarbitrary $ ArbDate (DT.date dt)
    coarbitrary $ ArbTime (DT.time dt)

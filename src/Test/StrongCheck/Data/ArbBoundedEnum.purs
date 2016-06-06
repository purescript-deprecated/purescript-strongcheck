module Test.StrongCheck.Data.ArbBoundedEnum where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum, toEnum, cardinality, runCardinality)
import Data.Int as Int
import Data.Maybe (fromJust)

import Partial.Unsafe (unsafePartial)

import Test.StrongCheck.Arbitrary (class Arbitrary, class Coarbitrary, coarbitrary)
import Test.StrongCheck.Gen (chooseInt)

newtype ArbBoundedEnum a = ArbBoundedEnum a

derive instance eqArbBoundedEnum :: Eq a => Eq (ArbBoundedEnum a)
derive instance ordArbBoundedEnum :: Ord a => Ord (ArbBoundedEnum a)

runArbBoundedEnum :: forall a. ArbBoundedEnum a -> a
runArbBoundedEnum (ArbBoundedEnum a) = a

instance arbArbBoundedEnum :: BoundedEnum a => Arbitrary (ArbBoundedEnum a) where
  arbitrary = ArbBoundedEnum <$> f (cardinality :: Cardinality a)
    where
    f = unsafePartial \(Cardinality sz) ->
      fromJust <<< toEnum <$> chooseInt 0.0 (Int.toNumber sz - 1.0)

instance coarbArbBoundedEnum :: BoundedEnum a => Coarbitrary (ArbBoundedEnum a) where
  coarbitrary (ArbBoundedEnum e) = coarbitrary (fromEnum e)

instance showArbBoundedEnum :: Show a => Show (ArbBoundedEnum a) where
  show (ArbBoundedEnum a) = "(ArbBoundedEnum " <> show a <> ")"

instance boundedArbBoundedEnum :: Bounded a => Bounded (ArbBoundedEnum a) where
  top = ArbBoundedEnum top
  bottom = ArbBoundedEnum bottom

instance enumHour :: BoundedEnum a => Enum (ArbBoundedEnum a) where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance enumArbBoundedEnum :: BoundedEnum a => BoundedEnum (ArbBoundedEnum a) where
  cardinality = Cardinality $ runCardinality (cardinality :: Cardinality a)
  toEnum v = ArbBoundedEnum <$> toEnum v
  fromEnum (ArbBoundedEnum e) = fromEnum e

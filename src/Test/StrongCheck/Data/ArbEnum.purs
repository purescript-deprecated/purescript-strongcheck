module Test.StrongCheck.Data.ArbEnum
  ( ArbEnum(..)
  , runArbEnum
  ) where

import Data.Enum (Enum, Cardinality(..), cardinality, fromEnum, toEnum, pred, succ)
import Data.Maybe.Unsafe (fromJust)
import Test.StrongCheck.Arbitrary
import Test.StrongCheck.Gen

newtype ArbEnum a = ArbEnum a

runArbEnum :: forall a. ArbEnum a -> a
runArbEnum (ArbEnum a) = a

instance arbArbEnum :: (Enum a) => Arbitrary (ArbEnum a) where
  arbitrary = ArbEnum <$> cardPerturb1 f where
    f (Cardinality sz) = fromJust <<< toEnum <$> chooseInt zero (sz - one)

instance coarbArbEnum :: (Enum a) => CoArbitrary (ArbEnum a) where
  coarbitrary (ArbEnum e) = coarbitrary (fromEnum e)

instance eqArbEnum :: (Eq a) => Eq (ArbEnum a) where
  (==) (ArbEnum a) (ArbEnum b) = a == b
  (/=) a b = not (a == b)

instance ordArbEnum :: (Ord a) => Ord (ArbEnum a) where
  compare (ArbEnum a) (ArbEnum b) = compare a b

instance boundedArbEnum :: (Bounded a) => Bounded (ArbEnum a) where
  top = ArbEnum top
  bottom = ArbEnum bottom

instance showArbEnum :: (Show a) => Show (ArbEnum a) where
  show (ArbEnum a) = "ArbEnum " ++ show a

instance enumArbEnum :: (Enum a) => Enum (ArbEnum a) where
  cardinality = arbEnumCardinality f where f (Cardinality sz) = Cardinality sz
  pred (ArbEnum e) = ArbEnum <$> pred e
  succ (ArbEnum e) = ArbEnum <$> succ e
  toEnum v = ArbEnum <$> toEnum v
  fromEnum (ArbEnum e) = fromEnum e

-- ScopedTypeVariables
cardPerturb1 :: forall f a. (Enum a) => (Cardinality a -> f a) -> f a
cardPerturb1 f = f cardinality

-- ScopedTypeVariables
arbEnumCardinality :: forall a. (Enum a) => (Cardinality a -> Cardinality (ArbEnum a)) -> Cardinality (ArbEnum a)
arbEnumCardinality f = f cardinality

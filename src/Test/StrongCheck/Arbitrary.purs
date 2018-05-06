module Test.StrongCheck.Arbitrary where

import Prelude

import Control.Monad.Gen as CMG
import Control.Monad.Gen.Common as CMGC

import Data.Array as A
import Data.Array.Partial as AP
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Int (fromNumber, toNumber)
import Data.Lazy (Lazy, defer, force)
import Data.List (List)
import Data.Maybe (Maybe(..), fromJust)
import Data.String as S
import Data.Tuple (Tuple(..))

import Math as Math

import Partial.Unsafe (unsafePartial)

import Test.StrongCheck.Gen (Gen, perturbGen, repeatable, oneOf, charGen, uniform)

-- | The `Arbitrary` class represents those types whose values can be
-- | _randomly-generated_.
-- |
-- | `arbitrary` uses the `Gen` monad to express a random generator for
-- | the type `t`. Combinators in the `Test.StrongCheck.Gen`
-- | module can be used to construct random generators.
class Arbitrary t where
  arbitrary :: Gen t

-- | The `Coarbitrary` class represents types which appear on the left of
-- | an `Arbitrary` function arrow.
-- |
-- | To construct an `Arbitrary` instance for the type `a -> b`, we need to
-- | use the input of type `a` to _perturb_ a random generator for `b`. This
-- | is the role of the `coarbitrary` function.
-- |
-- | `Coarbitrary` instances can be written using the `perturbGen` function.
class Coarbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r

instance arbBoolean :: Arbitrary Boolean where
  arbitrary = CMG.chooseBool

instance coarbBoolean :: Coarbitrary Boolean where
  coarbitrary true  = perturbGen 1.0
  coarbitrary false = perturbGen 2.0

instance arbNumber :: Arbitrary Number where
  arbitrary = uniform

instance coarbNumber :: Coarbitrary Number where
  coarbitrary = perturbGen

instance arbInt :: Arbitrary Int where
  arbitrary = unsafePartial do
    n <- uniform
    pure <<< fromJust <<< fromNumber <<< Math.floor $ toNumber ((top - bottom) + bottom) * n

instance coarbInt :: Coarbitrary Int where
  coarbitrary = perturbGen <<< toNumber

instance arbString :: Arbitrary String where
  arbitrary = S.fromCharArray <$> arbitrary

instance coarbString :: Coarbitrary String where
  coarbitrary s = coarbitrary $ (S.charCodeAt 0 <$> S.split (S.Pattern "") s)

instance arbChar :: Arbitrary Char where
  arbitrary = charGen

instance coarbChar :: Coarbitrary Char where
  coarbitrary c = coarbitrary $ toCharCode c

instance arbUnit :: Arbitrary Unit where
  arbitrary = pure unit

instance coarbUnit :: Coarbitrary Unit where
  coarbitrary _ = perturbGen 1.0

instance arbOrdering :: Arbitrary Ordering where
  arbitrary = oneOf (pure LT) [pure EQ, pure GT]

instance coarbOrdering :: Coarbitrary Ordering where
  coarbitrary LT = perturbGen 1.0
  coarbitrary EQ = perturbGen 2.0
  coarbitrary GT = perturbGen 3.0

instance arbArray :: Arbitrary a => Arbitrary (Array a) where
  arbitrary = do
    b <- arbitrary
    if b then pure [] else do
      a <- arbitrary
      as <- arbitrary
      pure $ [a] <> as

instance coarbArray :: Coarbitrary a => Coarbitrary (Array a) where
  coarbitrary = unsafePartial \arr ->
    if A.length arr == 0
    then identity
    else let x = AP.head arr
             xs = AP.tail arr
         in coarbitrary xs <<< coarbitrary x

instance arbFunction :: (Coarbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)

instance coarbFunction :: (Arbitrary a, Coarbitrary b) => Coarbitrary (a -> b) where
  coarbitrary f gen = do
    xs <- arbitrary
    coarbitrary (f xs) gen

instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = CMGC.genTuple arbitrary arbitrary

instance coarbTuple :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Tuple a b) where
  coarbitrary (Tuple a b) = coarbitrary a >>> coarbitrary b

instance arbMaybe :: Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = CMGC.genMaybe arbitrary

instance coarbMaybe :: Coarbitrary a => Coarbitrary (Maybe a) where
  coarbitrary Nothing = perturbGen 1.0
  coarbitrary (Just a) = coarbitrary a

instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = CMGC.genEither arbitrary arbitrary

instance coarbEither :: (Coarbitrary a, Coarbitrary b) => Coarbitrary (Either a b) where
  coarbitrary (Left a)  = coarbitrary a
  coarbitrary (Right b) = coarbitrary b

instance arbList :: Arbitrary a => Arbitrary (List a) where
  arbitrary = A.toUnfoldable <$> arbitrary

instance coarbList :: Coarbitrary a => Coarbitrary (List a) where
  coarbitrary = coarbitrary <<< A.fromFoldable

instance arbitraryIdentity :: Arbitrary a => Arbitrary (Identity a) where
  arbitrary = CMGC.genIdentity arbitrary

instance coarbIdentity :: Coarbitrary a => Coarbitrary (Identity a) where
  coarbitrary (Identity a) = coarbitrary a

instance arbitraryLazy :: Arbitrary a => Arbitrary (Lazy a) where
  arbitrary = arbitrary >>= pure <<< defer <<< const

instance coarbLazy :: Coarbitrary a => Coarbitrary (Lazy a) where
  coarbitrary a = coarbitrary (force a)

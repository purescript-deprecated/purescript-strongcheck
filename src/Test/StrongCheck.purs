module Test.StrongCheck
  ( (<?>)
  , (===)
  , (/==)
  , AlphaNumString(..)
  , Arbitrary
  , ArbEnum(..)
  , CoArbitrary
  , Negative(..)
  , NonZero(..)
  , Positive(..)
  , QC(..)
  , arbitrary
  , assert
  , coarbitrary
  , quickCheck
  , quickCheck'
  , quickCheckPure
  , Result(..)
  , runAlphaNumString
  , runArbEnum
  , runNegative
  , runNonZero
  , runPositive
  , runSignum
  , Signum(..)
  , smallCheck
  , smallCheckPure
  , statCheck
  , statCheckPure
  , test
  , Testable
  ) where

import Prelude

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Random (RANDOM(), random)
import Control.Monad.Eff.Exception (EXCEPTION(), throwException, error)
import Control.Monad.Trampoline
import Control.Bind

import Data.Foldable (Foldable)
import Data.Tuple (Tuple(..))
import Data.Int (fromNumber, toNumber)
import Data.Either
import Data.List
import Data.Maybe

import Data.Monoid
import Data.Enum
import Data.Traversable
import Math hiding (log)
import Data.Char

import qualified Data.Array.Unsafe as AU 
import qualified Data.String as S
import qualified Data.Array as A
import qualified Data.Maybe.Unsafe as MU

import Test.StrongCheck.Gen

class Arbitrary t where
  arbitrary :: Gen t

class CoArbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r

class Testable prop where
  test :: prop -> Gen Result

newtype AlphaNumString = AlphaNumString String

newtype Positive = Positive Number

newtype Negative = Negative Number

newtype NonZero  = NonZero Number

newtype Signum   = Signum Int

newtype ArbEnum a = ArbEnum a

type QC a = forall eff. Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) a

data Result = Success | Failed String

(<?>) :: Boolean -> String -> Result
(<?>) true  = const Success
(<?>) false = Failed

(===) :: forall a b. (Eq a, Show a) => a -> a -> Result
(===) a b = a == b <?> msg
  where msg = show a ++ " /= " ++ show b

(/==) :: forall a b. (Eq a, Show a) => a -> a -> Result
(/==) a b = a /= b <?> msg
  where msg = show a ++ " == " ++ show b

quickCheckPure :: forall prop. (Testable prop) => Int -> Seed -> prop -> Array Result
quickCheckPure n s prop = runTrampoline $ sample' n (defState s) (test prop)

quickCheck' :: forall prop. (Testable prop) => Int -> prop -> QC Unit
quickCheck' n prop = check (quickCheckPure n) prop

-- | Checks the proposition for 100 random values.
quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
quickCheck prop = quickCheck' 100 prop

smallCheckPure :: forall prop. (Testable prop) => Seed -> prop -> Array Result
smallCheckPure s prop = runTrampoline $ collectAll (defState s) (test prop)

-- | Exhaustively checks the proposition for all possible values. Assumes the
-- | generator is a finite generator.
smallCheck :: forall prop. (Testable prop) => prop -> QC Unit
smallCheck prop = check smallCheckPure prop

statCheckPure :: forall prop. (Testable prop) => Seed -> Number -> prop -> Result
statCheckPure s freq prop = try 100 where
  try :: Int -> Result 
  try x = let measure :: Int -> Number
              measure n = let results = quickCheckPure n s prop
                          in  toNumber $ (countSuccesses results) / (A.length results)

              measure' :: Int -> Array Number 
              measure' 0 = []
              measure' n = measure' (n - 1) <> [measure (n * x)]

              freqs :: Array Number
              freqs = measure' 4

              dists :: Array Number
              dists = (Math.abs <<< (-) freq) <$> freqs 

              dirs :: Array Number
              dirs  = A.zipWith (\a b -> a - b) ([1.0] <> dists) dists

              fails :: Int
              fails = A.length $ A.filter ((>) 0.0) dirs

              succs :: Array Number
              succs = A.filter ((<=) 0.0) dirs

          in  if fails > 1 then 
                if x < 1000000 then try (x * 10)
                else Failed $ "Divergence of statistical test: freqs = " ++ show freqs ++ ", dists = " ++ show dists ++ ", dirs = " ++ show dirs ++ ", fails: " ++ show fails
              else maybe (Failed "Error!") (\l -> if l > 0.5 then Failed $ "Final convergence distance too low: " ++ show l else Success) (A.last succs)

-- | Checks that the proposition has a certain probability of being true for 
-- | arbitrary values.
statCheck :: forall prop. (Testable prop) => Number -> prop -> QC Unit
statCheck freq prop = do
  seed <- random
  log <<< show $ statCheckPure seed freq prop

-- | Checks that the specified proposition holds. Useful for unit tests.
assert :: forall prop. (Testable prop) => prop -> QC Unit
assert = quickCheck' 1

defState :: Seed -> GenState
defState s = (GenState {seed: s, size: 10})

check :: forall prop f. (Testable prop, Foldable f) => (Seed -> prop -> f Result) -> prop -> QC Unit
check f prop = do
  seed <- random
  let results   = f seed prop
  let successes = countSuccesses results
  log $ show successes ++ "/" ++ show (length $ toList results) ++ " test(s) passed."
  throwOnFirstFailure 1 results

throwOnFirstFailure :: forall f. (Foldable f) => Int -> f Result -> QC Unit
throwOnFirstFailure n fr = throwOnFirstFailure' n (toList fr)
  where
  throwOnFirstFailure' :: Int -> List Result -> QC Unit
  throwOnFirstFailure' _ Nil = pure unit
  throwOnFirstFailure' n (Cons (Failed msg) _) = throwException $ error $ "Test " <> show n <> " failed: \n" <> msg
  throwOnFirstFailure' n (Cons _ rest) = throwOnFirstFailure (n + 1) rest


countSuccesses :: forall f. (Foldable f) => f Result -> Int
countSuccesses fa = countSuccesses' 0 (toList fa)
  where
  countSuccesses' acc Nil = acc
  countSuccesses' acc (Cons Success rest) = countSuccesses' (acc + 1) rest
  countSuccesses' acc (Cons _ rest) = countSuccesses' acc rest

maxNumber :: Number
maxNumber = 9007199254740992.0

runAlphaNumString (AlphaNumString s) = s

runSignum (Signum n) = n

runPositive (Positive n) = n

runNegative (Negative n) = n

runNonZero (NonZero n) = n

runArbEnum :: forall a. ArbEnum a -> a
runArbEnum (ArbEnum a) = a

instance eqResult :: Eq Result where
  eq Success Success = true
  eq (Failed m1) (Failed m2) = m1 == m2
  eq _ _ = false

instance showResult :: Show Result where
  show Success      = "Success"
  show (Failed msg) = "Failed: " ++ msg

instance semigroupResult :: Semigroup Result where
  append Success Success          = Success
  append (Failed msg) Success     = Failed msg
  append Success (Failed msg)     = Failed msg
  append (Failed m1) (Failed m2)  = Failed (m1 ++ "\n" ++ m2)

instance monoidResult :: Monoid Result where
  mempty = Success

instance arbNumber :: Arbitrary Number where
  arbitrary = uniform

instance arbInt :: Arbitrary Int where
  arbitrary = do
    n <- uniform
    pure <<< MU.fromJust <<< fromNumber <<< Math.floor $ toNumber ((top - bottom) + bottom) * n


instance coarbNumber :: CoArbitrary Number where
  coarbitrary = perturbGen

instance coarbInt :: CoArbitrary Int where
  coarbitrary = perturbGen <<< toNumber

instance arbPositive :: Arbitrary Positive where
  arbitrary = Positive <$> ((*) maxNumber) <$> uniform

instance coarbPositive :: CoArbitrary Positive where
  coarbitrary (Positive n) = coarbitrary n

instance arbNegative :: Arbitrary Negative where
  arbitrary = Negative <$> ((*) (-maxNumber)) <$> uniform

instance coarbNegative :: CoArbitrary Negative where
  coarbitrary (Negative n) = coarbitrary n

instance arbNonZero :: Arbitrary NonZero where
  arbitrary = do n <- arbitrary
                 b <- arbitrary
                 let sign = if b then 1.0 else -1.0
                 return $ NonZero (n * maxNumber * sign)

instance coarbNonZero :: CoArbitrary NonZero where
  coarbitrary (NonZero n) = coarbitrary n

instance arbSignum :: Arbitrary Signum where
  arbitrary = do b <- arbitrary
                 return $ Signum (if b then 1 else -1)

instance coarbSignum :: CoArbitrary Signum where
  coarbitrary (Signum n) = coarbitrary n

instance arbArbEnum :: (Enum a) => Arbitrary (ArbEnum a) where
  arbitrary = ArbEnum <$> cardPerturb1 f where
    f (Cardinality sz) = MU.fromJust <<< toEnum <$> chooseInt 0.0 (toNumber sz - 1.0)

instance coarbArbEnum :: (Enum a) => CoArbitrary (ArbEnum a) where
  coarbitrary (ArbEnum e) = coarbitrary (fromEnum e)

instance eqArbEnum :: (Eq a) => Eq (ArbEnum a) where
  eq (ArbEnum a) (ArbEnum b) = a == b


instance ordArbEnum :: (Ord a) => Ord (ArbEnum a) where
  compare (ArbEnum a) (ArbEnum b) = compare a b

instance showArbEnum :: (Show a) => Show (ArbEnum a) where
  show (ArbEnum a) = "ArbEnum " ++ show a

instance boundedArbEnum :: (Bounded a) => Bounded (ArbEnum a) where
  top = ArbEnum top
  bottom = ArbEnum bottom

instance enumArbEnum :: (Enum a) => Enum (ArbEnum a) where
  cardinality = arbEnumCardinality f where f (Cardinality sz) = Cardinality sz

  pred (ArbEnum e) = ArbEnum <$> pred e

  succ (ArbEnum e) = ArbEnum <$> succ e

  toEnum v = ArbEnum <$> toEnum v

  fromEnum (ArbEnum e) = fromEnum e

instance arbBoolean :: Arbitrary Boolean where
  arbitrary = do
    n <- uniform
    return $ (n < 0.5)

instance coarbBoolean :: CoArbitrary Boolean where
  coarbitrary true  = perturbGen 1.0
  coarbitrary false = perturbGen 2.0

instance arbChar :: Arbitrary Char where
  arbitrary = charGen

instance coarbChar :: CoArbitrary Char where
  coarbitrary c = coarbitrary $ toCharCode c

instance arbString :: Arbitrary String where
  arbitrary = S.fromCharArray <$> arbitrary

instance coarbString :: CoArbitrary String where
  coarbitrary s = coarbitrary $ (S.charCodeAt 0 <$> S.split "" s)

instance arbAlphaNumString :: Arbitrary AlphaNumString where
  arbitrary = do
    arrNum <- arbitrary
    return $ MU.fromJust $ (AlphaNumString <<< S.fromCharArray) <$> sequence (lookup <$> arrNum) where
      chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

      lookup x = S.charAt index chars where
        index = fromMaybe 0 $ fromNumber $ Math.round
                (Math.min (toNumber (S.length chars - 1)) (floor (x * toNumber (S.length chars))))

instance coarbAlphaNumString :: CoArbitrary AlphaNumString where
  coarbitrary (AlphaNumString s) = coarbitrary s

instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = Tuple <$> arbitrary <*> arbitrary

instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b) where
  coarbitrary (Tuple a b) = coarbitrary a >>> coarbitrary b

instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = do
    b <- arbitrary
    if b then Left <$> arbitrary else Right <$> arbitrary

instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b) where
  coarbitrary (Left a)  = coarbitrary a
  coarbitrary (Right b) = coarbitrary b

instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a) where
  arbitrary = do
    b <- arbitrary
    if b then pure Nothing else Just <$> arbitrary

instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a) where
  coarbitrary Nothing = perturbGen 1.0
  coarbitrary (Just a) = coarbitrary a

instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = repeatable (\a -> coarbitrary a arbitrary)

instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary f gen = do
    xs <- arbitrary
    coarbitrary (f xs) gen

instance arbArray :: (Arbitrary a) => Arbitrary (Array a) where
  arbitrary = do
    b <- arbitrary
    if b then return [] else do
      a <- arbitrary
      as <- arbitrary
      return $ [a] <> as

instance coarbArray :: (CoArbitrary a) => CoArbitrary (Array a) where
  coarbitrary arr =
    if A.length arr == 0
    then id
    else let x = AU.head arr
             xs = AU.tail arr
         in coarbitrary xs <<< coarbitrary x

instance testableResult :: Testable Result where
  test = return

instance testableBoolean :: Testable Boolean where
  test true = return Success
  test false = return $ Failed "Test returned false"

instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop) where
  test f = do
    t <- arbitrary
    test (f t)

-- ScopedTypeVariables
cardPerturb1 :: forall f a. (Enum a) => (Cardinality a -> f a) -> f a
cardPerturb1 f = f cardinality

-- ScopedTypeVariables
arbEnumCardinality :: forall a. (Enum a) => (Cardinality a -> Cardinality (ArbEnum a)) -> Cardinality (ArbEnum a)
arbEnumCardinality f = f cardinality


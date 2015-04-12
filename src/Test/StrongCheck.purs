module Test.StrongCheck
  ( (<?>)
  , (===)
  , (/==)
  , QC(..)
  , assert
  , quickCheck
  , quickCheck'
  , quickCheckPure
  , Result(..)
  , smallCheck
  , smallCheckPure
  , statCheck
  , statCheckPure
  , test
  , Testable
  ) where

import Debug.Trace
import Control.Monad.Eff
import Control.Monad.Trampoline
import Control.Bind
import Control.Monad.Eff.Random
import Control.Monad.Eff.Exception
import Data.Array
import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Monoid
import Data.Int
import Data.Enum
import Data.Traversable
import Math

import qualified Data.String as S
import Data.Char

import Test.StrongCheck.Arbitrary
import Test.StrongCheck.Gen
import Test.StrongCheck.LCG

class Testable prop where
  test :: prop -> Gen Result

type QC a = forall eff. Eff (trace :: Trace, random :: Random, err :: Exception | eff) a

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

quickCheckPure :: forall prop. (Testable prop) => Int -> Seed -> prop -> [Result]
quickCheckPure n s prop = runTrampoline $ sample' n (defState s) (test prop)

quickCheck' :: forall prop. (Testable prop) => Int -> prop -> QC Unit
quickCheck' n prop = check (quickCheckPure n) prop

-- | Checks the proposition for 100 random values.
quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
quickCheck prop = quickCheck' (fromNumber 100) prop

smallCheckPure :: forall prop. (Testable prop) => Int -> prop -> [Result]
smallCheckPure s prop = runTrampoline $ collectAll (defState s) (test prop)

-- | Exhaustively checks the proposition for all possible values. Assumes the
-- | generator is a finite generator.
smallCheck :: forall prop. (Testable prop) => prop -> QC Unit
smallCheck prop = check smallCheckPure prop

statCheckPure :: forall prop. (Testable prop) => Seed -> Number -> prop -> Result
statCheckPure s freq prop = try (fromNumber 100) where
  try x = let measure n = let results = quickCheckPure n s prop
                          in  (toNumber $ countSuccesses results) / (toNumber $ length results)

              measure' n | n == zero = []
                         | otherwise = measure' (n - one) <> [measure (n * x)]

              freqs = measure' (fromNumber 4)

              dists = (Math.abs <<< (-) freq) <$> freqs

              dirs  = zipWith (\a b -> a - b) (one : dists) dists

              fails = length $ filter ((>) zero) dirs

              succs = filter ((<=) zero) dirs

          in  if fails > one then
                if x < (fromNumber 1000000) then try (x * fromNumber 10)
                else Failed $ "Divergence of statistical test: freqs = " ++ show freqs ++ ", dists = " ++ show dists ++ ", dirs = " ++ show dirs ++ ", fails: " ++ show fails
              else maybe (Failed "Error!") (\l -> if l > 0.5 then Failed $ "Final convergence distance too low: " ++ show l else Success) (last succs)

-- | Checks that the proposition has a certain probability of being true for
-- | arbitrary values.
statCheck :: forall prop. (Testable prop) => Number -> prop -> QC Unit
statCheck freq prop = do
  seed <- randomSeed
  trace <<< show $ statCheckPure seed freq prop

-- | Checks that the specified proposition holds. Useful for unit tests.
assert :: forall prop. (Testable prop) => prop -> QC Unit
assert = quickCheck' one

defState :: Int -> GenState
defState s = GenState { seed: s, size: fromNumber 10 }

check :: forall prop. (Testable prop) => (Seed -> prop -> [Result]) -> prop -> QC Unit
check f prop = do
  seed <- randomSeed
  let results   = f seed prop
  let successes = countSuccesses results
  trace $ show successes ++ "/" ++ show (toNumber $ length results) ++ " test(s) passed."
  throwOnFirstFailure one results

throwOnFirstFailure :: Int -> [Result] -> QC Unit
throwOnFirstFailure _ []                  = return unit
throwOnFirstFailure n (Failed msg : _)    = throwException $ error $ "Test " ++ show n ++ " failed: \n" ++ msg
throwOnFirstFailure n (_          : rest) = throwOnFirstFailure (n + one) rest

countSuccesses :: [Result] -> Int
countSuccesses = countSuccesses' zero
  where countSuccesses' acc []               = acc
        countSuccesses' acc (Success : rest) = countSuccesses' (acc + one) rest
        countSuccesses' acc (_       : rest) = countSuccesses' acc rest

instance eqResult :: Eq Result where
  (==) Success Success = true
  (==) (Failed m1) (Failed m2) = m1 == m2
  (==) _ _ = false

  (/=) a b = not (a == b)

instance showResult :: Show Result where
  show Success      = "Success"
  show (Failed msg) = "Failed: " ++ msg

instance semigroupResult :: Semigroup Result where
  (<>) Success Success          = Success
  (<>) (Failed msg) Success     = Failed msg
  (<>) Success (Failed msg)     = Failed msg
  (<>) (Failed m1) (Failed m2)  = Failed (m1 ++ "\n" ++ m2)

instance monoidResult :: Monoid Result where
  mempty = Success

instance testableResult :: Testable Result where
  test = return

instance testableBoolean :: Testable Boolean where
  test true = return Success
  test false = return $ Failed "Test returned false"

instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop) where
  test f = do
    t <- arbitrary
    test (f t)

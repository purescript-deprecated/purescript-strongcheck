module Test.Main where

import Prelude

import Control.Monad.Trampoline (runTrampoline)

import Data.Array as Array
import Data.Foldable (fold, all, elem)
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Console (log)

import Math as Math

import Test.StrongCheck (Result, assert, statCheck, smallCheck, quickCheck, (<?>))
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen (sample, allInArray, foldGen, chooseInt, suchThat, chunked, collectAll, nChooseK, perms, infinite, takeGen, extend, elements, arrayOf, dropGen, allInRange, choose, arrayOf1)
import Test.StrongCheck.Landscape (Landscape, moveTo, sampleHere, somewhere)
import Test.StrongCheck.Perturb (searchIn)

data Mega = Mega {
  arrayOf     :: Array Number,
  arrayOf1    :: Array Number,
  choose      :: Number,
  chooseInt   :: Int,
  collectAll  :: Array Number,
  allInArray  :: Array Number,
  allInRange  :: Array Int,
  dropGen     :: Array Number,
  takeGen     :: Array Number,
  elements    :: Array String,
  extend      :: Array String,
  infinite    :: Array String,
  perms       :: Array (Array String),
  combos      :: Array (Array String),
  chunked     :: Array (Array String),
  suchThat    :: Int
}

{- TODO: Remaining cases
  , frequency
  , oneOf
  , perturbGen
  , repeatable
  , resize
  , sample
  , sample'
  , showSample
  , showSample'
  , sized
  , stateful
  , suchThat
  , suchThatMaybe
  , unfoldGen
  , uniform
  , variant
  , vectorOf
  -}

data DetABC = DetABC String

runDetABC :: DetABC -> String
runDetABC (DetABC s) = s

instance arbDetABC :: Arbitrary DetABC where
  arbitrary = DetABC <$> allInArray ["A", "B", "C"]

data OneToTen = OneToTen Int

runOneToTen :: OneToTen -> Number
runOneToTen (OneToTen n) = Int.toNumber n

instance arbOneToTen :: Arbitrary OneToTen where
  arbitrary = OneToTen <$> chooseInt 0 10

instance arbMega :: Arbitrary Mega where
  arbitrary = do
    arrayOf'    <- arrayOf (choose 0.0 10.0)
    arrayOf1'   <- arrayOf1 (choose 0.0 10.0)
    choose'     <- choose 0.0 10.0
    chooseInt'  <- chooseInt 0 10
    collectAll' <- collectAll mempty (allInArray [0.0, 1.0, 2.0])
    allInArray' <- collectAll mempty (allInArray [0.0, 1.0, 2.0])
    allInRange' <- collectAll mempty (allInRange 0 10)
    dropGen'    <- collectAll mempty $ dropGen 2 (allInArray [2.0, 1.0, -1.0])
    takeGen'    <- collectAll mempty $ takeGen 2 (allInArray [2.0, 1.0, -1.0])
    elements'   <- arrayOf $ elements "foo" $ List.fromFoldable ["bar", "baz"]
    extend'     <- collectAll mempty $ extend 3 (pure "5")
    infinite'   <- collectAll mempty $ takeGen 4 (infinite $ pure "foo")
    perms'      <- collectAll mempty $ perms ["John", "D"]
    combos'     <- collectAll mempty $ nChooseK 2 ["foo", "bar", "baz"]
    chunked'    <- collectAll mempty $ chunked 3 (pure "foo")
    suchThat'   <- suchThat (chooseInt 0 4) (_ /= 2)
    pure $ Mega {
      arrayOf:    arrayOf',
      arrayOf1:   (case arrayOf1' of Tuple a as -> [a] <> as),
      choose:     choose',
      chooseInt:  chooseInt',
      collectAll: collectAll',
      allInArray: allInArray',
      allInRange: allInRange',
      dropGen:    dropGen',
      takeGen:    takeGen',
      elements:   elements',
      extend:     extend',
      infinite:   infinite',
      perms:      perms',
      combos:     combos',
      chunked:    chunked',
      suchThat:   suchThat' }

verify_gen :: Mega -> Result
verify_gen (Mega m) = fold [
  all (between 0.0 10.0) m.arrayOf <?> "arrayOf: " <> show m.arrayOf,
  Array.length m.arrayOf1 >= 1 <?> "arrayOf1: " <> show m.arrayOf1,
  between 0.0 10.0 m.choose <?> "choose: " <> show m.choose,
  between 0.0 10.0 (Int.toNumber m.chooseInt) &&
  Math.floor (Int.toNumber m.chooseInt) == Int.toNumber m.chooseInt <?> "chooseInt: " <> show m.chooseInt,
  m.collectAll == [0.0, 1.0, 2.0] <?> "collectAll: " <> show m.collectAll,
  m.allInArray == [0.0, 1.0, 2.0] <?> "allInArray: " <> show m.allInArray,
  m.allInRange == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  <?> "allInRange: " <> show m.allInRange,
  m.dropGen == [-1.0] <?> "dropGen: " <> show m.dropGen,
  m.takeGen == [2.0, 1.0] <?> "takeGen: " <> show m.takeGen,
  all (flip elem ["foo", "bar", "baz"]) m.elements <?> "elements: " <> show m.elements,
  Array.take 3 m.extend == ["5", "5", "5"] <?> "extend: " <> show m.extend,
  m.infinite == ["foo", "foo", "foo", "foo"] <?> "infinite: " <> show m.infinite,
  m.perms == [["John", "D"], ["D", "John"]] <?> "perms: " <> show m.perms,
  m.combos == [["foo", "bar"], ["foo", "baz"], ["bar", "baz"]] <?> "combos: " <> show m.combos,
  m.chunked == [["foo", "foo", "foo"]] <?> "chunked: " <> show m.chunked,
  m.suchThat /= 2 <?> "suchThat: " <> show m.suchThat]

main :: Effect Unit
main = do
  log "Gen combinators"
  quickCheck $ verify_gen

  log "foldGen"
  quickCheck $ (runTrampoline $ foldGen (\a b -> Just $ a + b) 1 mempty (allInArray [1, 2, 3])) == 7

  log "smallCheck"
  smallCheck $ runDetABC >>> (flip elem ["A", "B", "C"])

  log "Fair distribution of booleans"
  statCheck (1.0/2.0) $ (==) true

  log "Fair distribution of ints"
  statCheck (1.0/11.0) $ runOneToTen >>> ((==) 1.0)

  log "search can find another example"
  let found n = Math.abs (n - 36.0) <= 2.0
  let v = runTrampoline $ sample 1 $ searchIn found 35.24
  log $ show v
  assert $ maybe false found (v Array.!! 0)

  log "creating infinite bool landscape doesn't blow the stack"
  assert $ isJust $ (somewhere 1.0 arbitrary :: Maybe (Landscape Boolean))

  log "sampling 100 values produces 100 values"
  assert $ ((==) 100 <<< Array.length <<< sampleHere 100 <$> (somewhere 1.0 arbitrary :: Maybe (Landscape Boolean))) == Just true

  log "Can move to sampled value"
  assert $ (do  l <- (somewhere 1.0 arbitrary :: Maybe (Landscape Boolean))
                x <- Array.head $ Array.drop 50 (sampleHere 100 l)
                l' <- moveTo x l
                pure true) == Just true

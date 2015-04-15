module Test.Main where

import Control.Monad.Eff (Eff())
import Control.Monad.Trampoline (runTrampoline)
import Data.Foldable (fold, all, elem)
import Data.Int (Int(), fromNumber)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Debug.Trace (trace)
import qualified Data.Array as Array
import Test.StrongCheck
import Test.StrongCheck.Arbitrary
import Test.StrongCheck.Gen
import Test.StrongCheck.Landscape
import Test.StrongCheck.Perturb

between :: forall a. (Ord a) => a -> a -> a -> Boolean
between min max = \n -> n >= min && n <= max

data Mega  = Mega {
  arrayOf     :: [Number],
  arrayOf1    :: [Number],
  choose      :: Number,
  chooseInt   :: Int,
  collectAll  :: [Number],
  allInArray  :: [Number],
  allInRange  :: [Number],
  dropGen     :: [Number],
  takeGen     :: [Number],
  elements    :: [String],
  extend      :: [String],
  infinite    :: [String],
  perms       :: [[String]],
  combos      :: [[String]],
  chunked     :: [[String]] }

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

runOneToTen :: OneToTen -> Int
runOneToTen (OneToTen n) = n

instance arbOneToTen :: Arbitrary OneToTen where
  arbitrary = OneToTen <$> chooseInt zero (fromNumber 10)

instance arbMega :: Arbitrary Mega where
  arbitrary = do
    arrayOf'    <- arrayOf (choose 0 10)
    arrayOf1'   <- arrayOf1 (choose 0 10)
    choose'     <- choose 0 10
    chooseInt'  <- chooseInt zero (fromNumber 10)
    collectAll' <- collectAll mempty (allInArray [0, 1, 2])
    allInArray' <- collectAll mempty (allInArray [0, 1, 2])
    allInRange' <- collectAll mempty (allInRange 0 10)
    dropGen'    <- collectAll mempty $ dropGen 2 (allInArray [2, 1, -1])
    takeGen'    <- collectAll mempty $ takeGen 2 (allInArray [2, 1, -1])
    elements'   <- arrayOf $ elements "foo" ["bar", "baz"]
    extend'     <- collectAll mempty $ extend (fromNumber 3) (pure "5")
    infinite'   <- collectAll mempty $ takeGen 4 (infinite $ pure "foo")
    perms'      <- collectAll mempty $ perms ["John", "D"]
    combos'     <- collectAll mempty $ nChooseK (fromNumber 2) ["foo", "bar", "baz"]
    chunked'    <- collectAll mempty $ chunked (fromNumber 3) (pure "foo")
    return $ Mega {
      arrayOf:    arrayOf',
      arrayOf1:   (case arrayOf1' of Tuple a as -> a : as),
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
      chunked:    chunked' }

verify_gen :: Mega -> Result
verify_gen (Mega m) = fold [
  all (between 0 10) m.arrayOf                          <?> "arrayOf: "     ++ show m.arrayOf,
  Array.length m.arrayOf1 >= one                        <?> "arrayOf1: "    ++ show m.arrayOf1,
  between 0 10 m.choose                                 <?> "choose: "      ++ show m.choose,
  between zero (fromNumber 10) m.chooseInt              <?> "chooseInt: "   ++ show m.chooseInt,
  m.collectAll == [0, 1, 2]                             <?> "collectAll: "  ++ show m.collectAll,
  m.allInArray == [0, 1, 2]                             <?> "allInArray: "  ++ show m.allInArray,
  m.allInRange == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]    <?> "allInRange: "  ++ show m.allInRange,
  m.dropGen == [-1]                                     <?> "dropGen: "     ++ show m.dropGen,
  m.takeGen == [2, 1]                                   <?> "takeGen: "     ++ show m.takeGen,
  all (flip elem ["foo", "bar", "baz"]) m.elements      <?> "elements: "    ++ show m.elements,
  Array.take (fromNumber 3) m.extend == ["5", "5", "5"] <?> "extend: "      ++ show m.extend,
  m.infinite == ["foo", "foo", "foo", "foo"]            <?> "infinite: "    ++ show m.infinite,
  m.perms == ["John", "D"] : ["D", "John"] : []         <?> "perms: "       ++ show m.perms,
  m.combos == ["foo", "bar"] : ["foo", "baz"] :
              ["bar", "baz"] : []                       <?> "combos: "      ++ show m.combos,
  m.chunked == ["foo", "foo", "foo"] : []               <?> "chunked: "     ++ show m.chunked]

main = do
  trace "Gen combinators"
  quickCheck $ verify_gen

  trace "foldGen"
  quickCheck $ (runTrampoline $ foldGen (\a b -> Just $ a + b) 1 mempty (allInArray [1, 2, 3])) == 7

  trace "smallCheck"
  smallCheck $ runDetABC >>> (flip elem ["A", "B", "C"])

  trace "Fair distribution of booleans"
  statCheck (1/2) $ (==) true

  trace "Fair distribution of ints"
  statCheck (1/11) $ runOneToTen >>> (== one)

  trace "search can find another example"
  let found n = Math.abs (n - 36) <= 2
  let v = runTrampoline $ sample one $ searchIn found 35.24
  trace $ show v
  assert $ maybe false found (v Array.!! zero)

  trace "creating infinite bool landscape doesn't blow the stack"
  assert $ isJust $ (somewhere 1 arbitrary :: Maybe (Landscape Boolean))

  trace "sampling 100 values produces 100 values"
  assert $ ((==) (fromNumber 100) <<< Array.length <<< sampleHere (fromNumber 100) <$> (somewhere 1 arbitrary :: Maybe (Landscape Boolean))) == Just true

  trace "Can move to sampled value"
  assert $ (do  l <- (somewhere 1 arbitrary :: Maybe (Landscape Boolean))
                x <- Array.head $ Array.drop (fromNumber 50) (sampleHere (fromNumber 100) l)
                l <- moveTo x l
                return true) == Just true

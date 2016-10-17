module Test.StrongCheck.Gen
  ( GenT(..)
  , Gen(..)
  , GenState(..)
  , GenOut(..)
  , Size
  , allInArray
  , allInRange
  , applyGen
  , arrayOf
  , arrayOf1
  , charGen
  , choose
  , chooseInt
  , chunked
  , collectAll
  , decorateSeed
  , dropGen
  , elements
  , extend
  , foldGen
  , foldGen'
  , frequency
  , infinite
  , interleave
  , nChooseK
  , oneOf
  , perms
  , perturbGen
  , repeatable
  , resize
  , runGen
  , sample
  , sample'
  , showSample
  , showSample'
  , shuffle
  , shuffle'
  , shuffleArray
  , sized
  , stateful
  , suchThat
  , suchThatMaybe
  , takeGen
  , toLazyList
  , transGen
  , uniform
  , unGen
  , unGenOut
  , unGenState
  , updateSeedState
  , variant
  , vectorOf
  , wrapEffect
  ) where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Lazy as CL
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (logShow, CONSOLE)
import Control.Monad.List.Trans as ListT
import Control.Monad.Trampoline (runTrampoline, Trampoline)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)

import Data.Array as A
import Data.Array.Partial as AP
import Data.Char (fromCharCode)
import Data.Foldable (fold)
import Data.Int (toNumber)
import Data.Lazy (Lazy, defer)
import Data.List as L
import Data.Machine.Mealy as Mealy
import Data.Maybe (fromMaybe, maybe, Maybe(..), fromJust)
import Data.Monoid (mempty, class Monoid)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Profunctor (arr, lmap)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..), snd, fst)

import Math as M

import Partial.Unsafe (unsafePartial)

import Test.StrongCheck.LCG (Seed, mkSeed, runSeed, lcgPerturb, lcgNext, lcgN)

type Size = Int

newtype GenState = GenState { seed :: Seed, size :: Size }

unGenState :: GenState -> { seed :: Seed, size :: Size }
unGenState (GenState s) = s

stateM
  :: ({ seed :: Seed, size :: Size } -> { seed :: Seed, size :: Size })
  -> GenState
  -> GenState
stateM f = GenState <<< f <<< unGenState

newtype GenOut a = GenOut { state :: GenState, value :: a }

unGenOut :: forall a. GenOut a -> { state :: GenState, value :: a }
unGenOut (GenOut v) = v

newtype GenT f a = GenT (Mealy.MealyT f GenState (GenOut a))

type Gen a = GenT Trampoline a

unGen :: forall f a. GenT f a -> Mealy.MealyT f GenState (GenOut a)
unGen (GenT m) = m

decorateSeed :: forall f a. Monad f => GenT f a -> GenT f (Tuple Seed a)
decorateSeed (GenT orig) = stateful \sIn ->
  GenT $ do
    GenOut x <- orig
    pure $ GenOut $ x { value = Tuple (unGenState sIn).seed x.value }

lcgStep :: forall f. Monad f => GenT f Int
lcgStep = GenT $ arr \s ->
  GenOut { state: updateSeedState s, value: runSeed (unGenState s).seed }

uniform :: forall f. Monad f => GenT f Number
uniform = (\n -> toNumber n / toNumber lcgN) <$> lcgStep

stepGen
  :: forall f a
   . Monad f
  => GenState
  -> GenT f a
  -> f (Maybe (GenOut (Tuple a (GenT f a))))
stepGen st (GenT m) = h <$> Mealy.stepMealy st m
  where
  h Mealy.Halt = Nothing
  h (Mealy.Emit a m) = Just $ flip Tuple (GenT m) <$> a

evalGen :: forall f a. Monad f => GenT f a -> GenState -> f (Maybe a)
evalGen g st = h <$> stepGen st g
  where
  h Nothing = Nothing
  h (Just (GenOut { value: Tuple a _ })) = Just a

pureGen :: forall f a. Monad f => (GenState -> GenOut a) -> GenT f a
pureGen f = GenT $ arr f

repeatable' :: forall f a b. Monad f => (a -> GenT f b) -> GenT f (a -> f b)
repeatable' = unsafePartial \f -> GenT $
  Mealy.pureMealy \s ->
    Mealy.Emit
      (GenOut { state: s, value: \a -> fromJust <$> evalGen (f a) s })
      Mealy.halt

-- | Creates a function generator that will always generate the same output
-- | for the same input.
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
repeatable f = g <$> repeatable' f
  where
  g f' = \a -> runTrampoline $ f' a

-- | Creates a generator that depends on access to the generator state.
stateful :: forall f a. Monad f => (GenState -> GenT f a) -> GenT f a
stateful f = GenT $ unGen <<< f =<< Mealy.take 1 id

-- | Fixes a generator on a certain variant, given by the specified seed.
variant :: forall f a. Monad f => Seed -> GenT f a -> GenT f a
variant n g = GenT $ lmap (stateM (\s -> s { seed = n })) (unGen g)

-- | Creates a generator that depends on the size parameter.
sized :: forall f a. Monad f => (Size -> GenT f a) -> GenT f a
sized f = stateful \s -> f (unGenState s).size

-- | Resizes the generator so the size parameter passed into the generator
-- | will be equal to the specified size.
resize :: forall f a. Monad f => Size -> GenT f a -> GenT f a
resize sz g = GenT $ lmap (stateM (\s -> s { size = sz })) (unGen g)

-- | A generator for characters.
charGen :: forall f. Monad f => GenT f Char
charGen = fromCharCode <$> chooseInt 0 65535

-- | Creates a generator that generates real numbers between the specified
-- | inclusive range.
choose :: forall f. Monad f => Number -> Number -> GenT f Number
choose a b = (*) (max - min) >>> (+) min <$> uniform
  where
  min = M.min a b
  max = M.max a b

-- | Creates a generator that generates integers between the specified
-- | inclusive range.
chooseInt :: forall f. Monad f => Int -> Int -> GenT f Int
chooseInt a b = clamp <$> lcgStep
  where
  clamp :: Int -> Int
  clamp x = case x `mod` (b - a + one) of
              r | r >= 0 -> a + r
                | otherwise -> b + r + one

-- | Creates a generator that chooses another generator from the specified list
-- | at random, and then generates a value with that generator.
oneOf :: forall f a. Monad f => GenT f a -> Array (GenT f a) -> GenT f a
oneOf x xs = do
  n <- chooseInt 0 (A.length xs)
  if n == 0 then x else fromMaybe x (xs A.!! (n - 1))

-- | Generates elements by the specified frequencies (which will be normalized).
frequency
  :: forall f a
   . Monad f
  => Tuple Number (GenT f a)
  -> L.List (Tuple Number (GenT f a))
  -> GenT f a
frequency x xs = do
  let xxs :: L.List (Tuple Number (GenT f a))
      xxs = L.Cons x xs

      total :: Number
      total = unwrap $ fold ((Additive <<< fst) <$> xxs)

      pick :: Number -> GenT f a -> L.List (Tuple Number (GenT f a)) -> GenT f a
      pick _ d L.Nil = d
      pick n d (L.Cons (Tuple k x) xs) = if n <= k then x else pick (n - k) d xs

  n <- choose 1.0 total
  pick n (snd x) xxs

-- | Creates a generator of elements ranging from 0 to the maximum size.
arrayOf :: forall f a. Monad f => GenT f a -> GenT f (Array a)
arrayOf g = sized \n -> do
  k <- chooseInt 0 n
  vectorOf k g

-- | Creates a generator of elements ranging from 1 to the maximum size.
arrayOf1 :: forall f a. Monad f => GenT f a -> GenT f (Tuple a (Array a))
arrayOf1 g = sized \n -> do
  k <- chooseInt 0 n
  x <- g
  xs <- vectorOf (k - 1) g
  pure $ Tuple x xs

-- | Creates a generator that generates arrays of some specified size.
vectorOf :: forall f a. Monad f => Int -> GenT f a -> GenT f (Array a)
vectorOf n g = transGen f [] (extend n g)
  where
  f b a =
    let b' = b <> [a]
    in if A.length b' >= n then Tuple [] (Just b') else Tuple b' Nothing

-- | Creates a generator that chooses an element from among a set of elements.
elements :: forall f a. Monad f => a -> L.List a -> GenT f a
elements x xs = do
  n <- chooseInt 0 (L.length xs)
  pure if n == 0 then x else fromMaybe x (xs L.!! (n - 1))

foreign import float32ToInt32 :: Number -> Int

perturbGen :: forall f a. Monad f => Number -> GenT f a -> GenT f a
perturbGen n (GenT m) =
  GenT $ lmap (stateM (\s -> s { seed = lcgPerturb (toNumber (float32ToInt32 n)) s.seed })) m

updateSeedState :: GenState -> GenState
updateSeedState (GenState s) = GenState { seed: lcgNext s.seed, size: s.size }

updateSeedGen :: forall f a. Monad f => GenT f a -> GenT f a
updateSeedGen (GenT m) = GenT $ lmap updateSeedState m

liftMealy
  :: forall f a
   . Monad f
   => (Mealy.MealyT f GenState (GenOut a) -> Mealy.MealyT f GenState (GenOut a))
   -> GenT f a
   -> GenT f a
liftMealy f = \g -> GenT $ f (unGen g)

-- | Takes the first number of values from the generator. Will turn an infinite
-- | generator into a finite generator.
takeGen :: forall f a. Monad f => Int -> GenT f a -> GenT f a
takeGen n = liftMealy $ Mealy.take n

-- | Drops a certain number of values from the generator. May produce
-- | an empty generator if called on a finite generator.
dropGen :: forall f a. Monad f => Int -> GenT f a -> GenT f a
dropGen n = liftMealy $ Mealy.drop n

-- | Extends a generator to produce *at least* the specified number of values.
-- | Will not turn a finite generator into an infinite one.
extend :: forall f a. Monad f => Int -> GenT f a -> GenT f a
extend n (GenT m) = (GenT $ loop 0 m) <> (GenT m)
  where
  m0 = m
  loop i m = Mealy.mealy \st ->
    let
      f (Mealy.Emit s m) = pure $ Mealy.Emit s (loop (i + 1) m)
      f Mealy.Halt =
        if i >= n then pure Mealy.Halt else Mealy.stepMealy st (loop i m0)
    in Mealy.stepMealy st m >>= f

-- | Fairly interleaves two generators.
interleave :: forall f a. Monad f => GenT f a -> GenT f a -> GenT f a
interleave (GenT g1) (GenT g2) = GenT $ Mealy.interleave g1 g2

-- | Ensures that a given generator can produce an infinite number of values,
-- | assuming it can produce at least one.
infinite :: forall f a. Monad f => GenT f a -> GenT f a
infinite = liftMealy $ Mealy.loop

-- | Folds over a generator to produce a value. Either the generator or the
-- | user-defined function may halt the fold. Returns not just the value
-- | created through folding, but also the successor generator.
foldGen'
  :: forall f a b
   . Monad f
  => (b -> a -> Maybe b)
  -> b
  -> GenState
  -> GenT f a -> f (Tuple b (GenT f a))
foldGen' f b s (GenT m) = loop s m b
  where
  loop st m b = Mealy.stepMealy st m >>= g
    where
    g Mealy.Halt = pure $ Tuple b (GenT Mealy.halt)
    g (Mealy.Emit (GenOut { value: a, state: st }) m) =
      let b' = f b a in maybe (pure $ Tuple b (GenT m)) (loop st m) b'

-- | Folds over a generator to produce a value. Either the generator or the
-- | user-defined function may halt the fold.
foldGen
  :: forall f a b
   . Monad f
  => (b -> a -> Maybe b)
  -> b
  -> GenState
  -> GenT f a
  -> f b
foldGen f b s g = fst <$> foldGen' f b s g

-- | Transforms one gen into another, passing along user-supplied state.
-- | Either the generator being transformed or the transforming function may
-- | halt the transformation.
transGen
  :: forall f a b c
   . Monad f
  => (b -> a -> Tuple b (Maybe c))
  -> b
  -> GenT f a
  -> GenT f c
transGen f b (GenT m) = GenT $ loop m b
  where
  loop m b = Mealy.mealy \st -> Mealy.stepMealy st m >>= g
    where
    g Mealy.Halt = pure Mealy.Halt
    g (Mealy.Emit (GenOut { value: a, state: st }) m) =
      case f b a of
        Tuple b Nothing  -> Mealy.stepMealy st (loop m b)
        Tuple b (Just c) ->
          let c' = GenOut { value: c, state: st }
          in  pure $ Mealy.Emit c' (loop m b)

-- | A deterministic generator that produces all possible permutations of
-- | the specified array.
perms :: forall f a. Monad f => Array a -> GenT f (Array a)
perms = unsafePartial \arr ->
  if A.length arr == 0
  then pure [ ]
  else do
    let hd = AP.head arr
        tl = AP.tail arr
    xs <- perms tl
    let f n = let prefix = A.take n xs
                  suffix = A.drop n xs
              in prefix <> [hd] <> suffix
    allInArray $ f <$> A.range 0 (A.length xs)

-- | A deterministic generator that produces all possible combinations of
-- | choosing exactly k elements from the specified array.
nChooseK :: forall f a. Monad f => Int -> Array a -> GenT f (Array a)
nChooseK = unsafePartial \k arr ->
  if k == 0 then pure []
  else if A.length arr == 0
       then mempty
       else
         let tl = A.drop 1 arr
             hd = A.singleton $ A.unsafeIndex arr 0
         in ((hd <> _) <$> (nChooseK (k - 1) tl)) <> nChooseK k tl

-- | Same as `nChooseK` but for `List`
nChooseKL :: forall f a. Monad f => Int -> L.List a -> GenT f (L.List a)
nChooseKL 0 _ = pure L.Nil
nChooseKL _ L.Nil = mempty
nChooseKL k (L.Cons x xs) =
  (((L.singleton x) <> _) <$> (nChooseKL (k - 1) xs)) <> nChooseKL k xs

-- | Filters a generator to produce only values satisfying the specified
-- | predicate.
suchThat :: forall f a. Monad f => GenT f a -> (a -> Boolean) -> GenT f a
suchThat g p = g `suchThatMaybe` p >>= maybe (sized next) pure
  where
  next n = resize (n + 1) (g `suchThat` p)

-- | Tries to filter a generator such that it only produces values satisfying
-- | the specified predicate.
suchThatMaybe
  :: forall f a
   . Monad f
  => GenT f a
  -> (a -> Boolean)
  -> GenT f (Maybe a)
suchThatMaybe g p = sized $ try 0
  where
  try _ 0 = pure Nothing
  try k n = do
    x <- resize (2 * k + n) g
    if p x then pure (Just x) else try (k + 1) (n - 1)

-- | A deterministic generator that produces integers from the specified
-- | inclusive range, in sequence.
allInRange :: forall f. Monad f => Int -> Int -> GenT f Int
allInRange min max = GenT $ go min
  where
  go cur = Mealy.pureMealy \s ->
    if cur > max
    then Mealy.Halt
    else Mealy.Emit (GenOut {state:s, value:cur}) (go (cur + 1))

-- | A deterministic generator that produces values from the specified array,
-- | in sequence.
allInArray :: forall f a. Monad f => Array a -> GenT f a
allInArray a = GenT $ go 0
  where
  go i = Mealy.pureMealy \s ->
    maybe
      Mealy.Halt
      (\a -> Mealy.Emit (GenOut { state: s, value: a }) (go (i + 1)))
      (a A.!! i)

-- | Drains a finite generator of all values. Or blows up if you called it on
-- | an infinite generator.
collectAll :: forall f a. Monad f => GenState -> GenT f a -> f (Array a)
collectAll = foldGen f []
  where
  f v a = Just $ v <> [a]

-- | Applies a state to a generator to possibly produce the next state,
-- | a value, and the next generator.
applyGen
  :: forall f a
   . Monad f
  => GenState
  -> GenT f a
  -> f (Maybe (GenOut (Tuple a (GenT f a))))
applyGen s (GenT m) = f <$> Mealy.stepMealy s m
  where
  f Mealy.Halt = Nothing
  f (Mealy.Emit (GenOut { state: s, value: a }) m) =
    Just $ GenOut { state: s, value: Tuple a (GenT m)}

-- | Samples a generator, producing the specified number of values.
sample' :: forall f a. Monad f => Int -> GenState -> GenT f a -> f (Array a)
sample' n st g = fst <$> runGen n st g

-- | Samples a generator, producing the specified number of values. Uses
-- | default settings for the initial generator state.
sample :: forall f a. Monad f => Int -> GenT f a -> f (Array a)
sample n = sample' n (GenState { size: 10, seed: mkSeed 445645874 })

-- | Shows a sample of values generated from the specified generator.
showSample'
  :: forall r a
   . Show a
  => Int
  -> Gen a
  -> Eff (console :: CONSOLE | r) Unit
showSample' n g = logShow $ runTrampoline $ sample n g

-- | Shows a sample of values generated from the specified generator.
showSample :: forall r a. Show a => Gen a -> Eff (console :: CONSOLE | r) Unit
showSample = showSample' 10

-- | Runs a generator to produce a specified number of values, returning both
-- | an array containing the values and the successor Gen that can be used to
-- | continue the generation process at a later time.
runGen
  :: forall f a
   . Monad f
  => Int
  -> GenState
  -> GenT f a -> f (Tuple (Array a) (GenT f a))
runGen n st g = foldGen' f [] st (extend n g)
  where
  f v a = if A.length v < n then Just $ v <> [a] else Nothing

-- | Creates a generator that produces chunks of values in the specified size.
-- | Will extend the generator if necessary to produce a chunk of the specified
-- | size, but will not turn a finite generator into an infinite generator.
chunked :: forall f a. Monad f => Int -> GenT f a -> GenT f (Array a)
chunked n g = transGen f [] (extend n g)
  where
  f xs a =
    let xs' = a A.: xs
    in if A.length xs' >= n then Tuple [] (Just xs') else Tuple xs' Nothing

-- | Wraps an effect in a generator that ignores the input state.
wrapEffect :: forall f a. Monad f => f a -> GenT f a
wrapEffect fa = GenT $ g <$> (id &&& (Mealy.wrapEffect fa))
  where
  g (Tuple l r) = GenOut { state: l, value: r }

-- | Creates a generator that mixes up the order of the specified generator.
-- | This is achieved by chunking the generator with the specified size
-- | and then shuffling each chunk before continuing to the next.
shuffle' :: forall f a. Monad f => Int -> GenT f a -> GenT f a
shuffle' n g = do
  chunks <- chunked n g
  shuffled <- shuffleArray chunks
  allInArray shuffled

-- | Same as shuffle' but with default for the chunk size.
shuffle :: forall f a. Monad f => GenT f a -> GenT f a
shuffle = shuffle' 100

-- | Creates a generator that produces shuffled versions of the provided array.
shuffleArray :: forall f a. Monad f => Array a -> GenT f (Array a)
shuffleArray = shuffle0 []
  where
  shuffle0 acc [] = pure $ acc
  shuffle0 acc xs = do
    i <- chooseInt 0 (A.length xs - 1)
    let acc' = acc <> (maybe [] A.singleton (xs A.!! i))
        xs' = fromMaybe xs $ A.deleteAt i xs
    shuffle0 acc' xs'

-- | Converts the generator into a function that, given the initial state,
-- | returns a lazy list.
toLazyList :: forall a. Gen a -> GenState -> ListT.ListT Lazy a
toLazyList (GenT m) s = ListT.wrapLazy $ defer \_ -> loop m s
  where
  loop m s =
    case runTrampoline (Mealy.stepMealy s m) of
      Mealy.Halt -> ListT.nil
      Mealy.Emit (GenOut { value: a, state: s }) m ->
        ListT.prepend' a (defer \_ -> loop m s)

instance semigroupGenState :: Semigroup GenState where
  append (GenState a) (GenState b) =
    GenState { seed: lcgPerturb (toNumber (runSeed a.seed)) b.seed, size: b.size }

instance monoidGenState :: Monoid GenState where
  mempty = GenState { seed: mkSeed 85734629, size: 10 }

instance semigroupGenOut :: Semigroup a => Semigroup (GenOut a) where
  append (GenOut a) (GenOut b) =
    GenOut { state: a.state <> b.state, value: a.value <> b.value }

instance monoidGenOut :: Monoid a => Monoid (GenOut a) where
  mempty = GenOut { state: mempty, value: mempty }

instance functorGenOut :: Functor GenOut where
  map f (GenOut m) = GenOut { state: m.state, value: f m.value }

instance applyGenOut :: Apply GenOut where
  apply (GenOut f) (GenOut x) =
    GenOut { state: x.state, value: f.value x.value }

-- GenT instances
instance functorGenT :: Monad f => Functor (GenT f) where
  map f (GenT m) = GenT $ map f <$> m

instance applyGenT :: Monad f => Apply (GenT f) where
  apply f x = GenT do
    f <- unGen f
    x <- unGen $ updateSeedGen x
    pure $ f <*> x

instance applicativeGenT :: Monad f => Applicative (GenT f) where
  pure t = GenT $ arr (\s -> GenOut { state: updateSeedState s, value: t })

instance semigroupGenT :: Monad f => Semigroup (GenT f a) where
  append (GenT a) (GenT b) = GenT (a <> b)

instance monoidGenT :: Monad f => Monoid (GenT f a) where
  mempty = GenT mempty

instance bindGenT :: Monad f => Bind (GenT f) where
  bind (GenT m) f = GenT do
    a <- m
    unGen $ updateSeedGen (f (unGenOut a).value)

instance monadGenT :: Monad f => Monad (GenT f)

instance altGenT :: Monad f => Alt (GenT f) where
  alt (GenT x) (GenT y) = GenT $ x <|> y

instance plusGenT :: Monad f => Plus (GenT f) where
  empty = mempty

instance alternativeGenT :: Monad f => Alternative (GenT f)

instance monadZeroGenT :: Monad f => MonadZero (GenT f)

instance monadPlusGenT :: Monad f => MonadPlus (GenT f)

instance lazyGenT :: Monad f => CL.Lazy (GenT f a) where
  defer f = GenT $ CL.defer (unGen <<< f)

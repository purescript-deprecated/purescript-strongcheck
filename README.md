# Module Documentation

## Module Test.StrongCheck

#### `Arbitrary`

``` purescript
class Arbitrary t where
  arbitrary :: Gen t
```


#### `CoArbitrary`

``` purescript
class CoArbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r
```


#### `Testable`

``` purescript
class Testable prop where
  test :: prop -> Gen Result
```


#### `AlphaNumString`

``` purescript
newtype AlphaNumString
  = AlphaNumString String
```


#### `Positive`

``` purescript
newtype Positive
  = Positive Number
```


#### `Negative`

``` purescript
newtype Negative
  = Negative Number
```


#### `NonZero`

``` purescript
newtype NonZero
  = NonZero Number
```


#### `Signum`

``` purescript
newtype Signum
  = Signum Number
```


#### `ArbEnum`

``` purescript
newtype ArbEnum a
  = ArbEnum a
```


#### `QC`

``` purescript
type QC a = forall eff. Eff (err :: Exception, random :: Random, trace :: Trace | eff) a
```


#### `Result`

``` purescript
data Result
  = Success 
  | Failed String
```


#### `(<?>)`

``` purescript
(<?>) :: Boolean -> String -> Result
```


#### `(===)`

``` purescript
(===) :: forall a b. (Eq a, Show a) => a -> a -> Result
```


#### `(/==)`

``` purescript
(/==) :: forall a b. (Eq a, Show a) => a -> a -> Result
```


#### `quickCheckPure`

``` purescript
quickCheckPure :: forall prop. (Testable prop) => Number -> Seed -> prop -> [Result]
```


#### `quickCheck'`

``` purescript
quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit
```


#### `quickCheck`

``` purescript
quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
```

Checks the proposition for 100 random values.

#### `smallCheckPure`

``` purescript
smallCheckPure :: forall prop. (Testable prop) => Number -> prop -> [Result]
```


#### `smallCheck`

``` purescript
smallCheck :: forall prop. (Testable prop) => prop -> QC Unit
```

Exhaustively checks the proposition for all possible values. Assumes the
generator is a finite generator.

#### `statCheckPure`

``` purescript
statCheckPure :: forall prop. (Testable prop) => Seed -> Number -> prop -> Result
```


#### `statCheck`

``` purescript
statCheck :: forall prop. (Testable prop) => Number -> prop -> QC Unit
```

Checks that the proposition has a certain probability of being true for 
arbitrary values.

#### `assert`

``` purescript
assert :: forall prop. (Testable prop) => prop -> QC Unit
```

Checks that the specified proposition holds. Useful for unit tests.

#### `runArbEnum`

``` purescript
runArbEnum :: forall a. ArbEnum a -> a
```


#### `eqResult`

``` purescript
instance eqResult :: Eq Result
```


#### `showResult`

``` purescript
instance showResult :: Show Result
```


#### `semigroupResult`

``` purescript
instance semigroupResult :: Semigroup Result
```


#### `monoidResult`

``` purescript
instance monoidResult :: Monoid Result
```


#### `arbNumber`

``` purescript
instance arbNumber :: Arbitrary Number
```


#### `coarbNumber`

``` purescript
instance coarbNumber :: CoArbitrary Number
```


#### `arbPositive`

``` purescript
instance arbPositive :: Arbitrary Positive
```


#### `coarbPositive`

``` purescript
instance coarbPositive :: CoArbitrary Positive
```


#### `arbNegative`

``` purescript
instance arbNegative :: Arbitrary Negative
```


#### `coarbNegative`

``` purescript
instance coarbNegative :: CoArbitrary Negative
```


#### `arbNonZero`

``` purescript
instance arbNonZero :: Arbitrary NonZero
```


#### `coarbNonZero`

``` purescript
instance coarbNonZero :: CoArbitrary NonZero
```


#### `arbSignum`

``` purescript
instance arbSignum :: Arbitrary Signum
```


#### `coarbSignum`

``` purescript
instance coarbSignum :: CoArbitrary Signum
```


#### `arbArbEnum`

``` purescript
instance arbArbEnum :: (Enum a) => Arbitrary (ArbEnum a)
```


#### `coarbArbEnum`

``` purescript
instance coarbArbEnum :: (Enum a) => CoArbitrary (ArbEnum a)
```


#### `eqArbEnum`

``` purescript
instance eqArbEnum :: (Eq a) => Eq (ArbEnum a)
```


#### `ordArbEnum`

``` purescript
instance ordArbEnum :: (Ord a) => Ord (ArbEnum a)
```


#### `showArbEnum`

``` purescript
instance showArbEnum :: (Show a) => Show (ArbEnum a)
```


#### `enumArbEnum`

``` purescript
instance enumArbEnum :: (Enum a) => Enum (ArbEnum a)
```


#### `arbBoolean`

``` purescript
instance arbBoolean :: Arbitrary Boolean
```


#### `coarbBoolean`

``` purescript
instance coarbBoolean :: CoArbitrary Boolean
```


#### `arbChar`

``` purescript
instance arbChar :: Arbitrary Char
```


#### `coarbChar`

``` purescript
instance coarbChar :: CoArbitrary Char
```


#### `arbString`

``` purescript
instance arbString :: Arbitrary String
```


#### `coarbString`

``` purescript
instance coarbString :: CoArbitrary String
```


#### `arbAlphaNumString`

``` purescript
instance arbAlphaNumString :: Arbitrary AlphaNumString
```


#### `coarbAlphaNumString`

``` purescript
instance coarbAlphaNumString :: CoArbitrary AlphaNumString
```


#### `arbTuple`

``` purescript
instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)
```


#### `coarbTuple`

``` purescript
instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b)
```


#### `arbEither`

``` purescript
instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
```


#### `coarbEither`

``` purescript
instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b)
```


#### `arbMaybe`

``` purescript
instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a)
```


#### `coarbMaybe`

``` purescript
instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a)
```


#### `arbFunction`

``` purescript
instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)
```


#### `coarbFunction`

``` purescript
instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)
```


#### `arbArray`

``` purescript
instance arbArray :: (Arbitrary a) => Arbitrary [a]
```


#### `coarbArray`

``` purescript
instance coarbArray :: (CoArbitrary a) => CoArbitrary [a]
```


#### `testableResult`

``` purescript
instance testableResult :: Testable Result
```


#### `testableBoolean`

``` purescript
instance testableBoolean :: Testable Boolean
```


#### `testableFunction`

``` purescript
instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop)
```



## Module Test.StrongCheck.Gen

#### `Size`

``` purescript
type Size = Number
```


#### `Seed`

``` purescript
type Seed = Number
```


#### `GenState`

``` purescript
data GenState
  = GenState { size :: Size, seed :: Seed }
```


#### `unGenState`

``` purescript
unGenState :: GenState -> { size :: Size, seed :: Seed }
```


#### `GenOut`

``` purescript
data GenOut a
  = GenOut { value :: a, state :: GenState }
```


#### `unGenOut`

``` purescript
unGenOut :: forall a. GenOut a -> { value :: a, state :: GenState }
```


#### `GenT`

``` purescript
data GenT f a
  = GenT (Mealy.MealyT f GenState (GenOut a))
```


#### `Gen`

``` purescript
type Gen a = GenT Trampoline a
```


#### `unGen`

``` purescript
unGen :: forall f a. GenT f a -> Mealy.MealyT f GenState (GenOut a)
```


#### `uniform`

``` purescript
uniform :: forall f. (Monad f) => GenT f Seed
```


#### `repeatable`

``` purescript
repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)
```

Creates a function generator that will always generate the same output
for the same input.

#### `stateful`

``` purescript
stateful :: forall f a. (Monad f) => (GenState -> GenT f a) -> GenT f a
```

Creates a generator that depends on access to the generator state.

#### `variant`

``` purescript
variant :: forall f a. (Monad f) => Seed -> GenT f a -> GenT f a
```

Fixes a generator on a certain variant, given by the specified seed.

#### `sized`

``` purescript
sized :: forall f a. (Monad f) => (Size -> GenT f a) -> GenT f a
```

Creates a generator that depends on the size parameter.

#### `resize`

``` purescript
resize :: forall f a. (Monad f) => Size -> GenT f a -> GenT f a
```

Resizes the generator so the size parameter passed into the generator
will be equal to the specified size.

#### `charGen`

``` purescript
charGen :: forall f. (Monad f) => GenT f Char
```

A generator for characters.

#### `choose`

``` purescript
choose :: forall f. (Monad f) => Number -> Number -> GenT f Number
```

Creates a generator that generates real numbers between the specified
inclusive range.

#### `chooseInt`

``` purescript
chooseInt :: forall f. (Monad f) => Number -> Number -> GenT f Number
```

Creates a generator that generates integers between the specified
inclusive range.

#### `oneOf`

``` purescript
oneOf :: forall f a. (Monad f) => GenT f a -> [GenT f a] -> GenT f a
```

Creates a generator that chooses another generator from the specified list
at random, and then generates a value with that generator.

#### `frequency`

``` purescript
frequency :: forall f a. (Monad f) => Tuple Number (GenT f a) -> [Tuple Number (GenT f a)] -> GenT f a
```

Generates elements by the specified frequencies (which will be normalized).

#### `arrayOf`

``` purescript
arrayOf :: forall f a. (Monad f) => GenT f a -> GenT f [a]
```

Creates a generator of elements ranging from 0 to the maximum size.

#### `arrayOf1`

``` purescript
arrayOf1 :: forall f a. (Monad f) => GenT f a -> GenT f (Tuple a [a])
```

Creates a generator of elements ranging from 1 to the maximum size.

#### `vectorOf`

``` purescript
vectorOf :: forall f a. (Monad f) => Number -> GenT f a -> GenT f [a]
```

Creates a generator that generates arrays of some specified size.

#### `elements`

``` purescript
elements :: forall f a. (Monad f) => a -> [a] -> GenT f a
```

Creates a generator that chooses an element from among a set of elements.

#### `perturbGen`

``` purescript
perturbGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
```


#### `updateSeedState`

``` purescript
updateSeedState :: GenState -> GenState
```


#### `takeGen`

``` purescript
takeGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
```

Takes the first number of values from the generator. Will turn an infinite
generator into a finite generator.

#### `dropGen`

``` purescript
dropGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
```

Drops a certain number of values from the generator. May produce
an empty generator if called on a finite generator.

#### `extend`

``` purescript
extend :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
```

Extends a generator to produce *at least* the specified number of values.
Will not turn a finite generator into an infinite one.

#### `interleave`

``` purescript
interleave :: forall f a. (Monad f) => GenT f a -> GenT f a -> GenT f a
```

Fairly interleaves two generators.

#### `infinite`

``` purescript
infinite :: forall f a. (Monad f) => GenT f a -> GenT f a
```

Ensures that a given generator can produce an infinite number of values,
assuming it can produce at least one.

#### `foldGen'`

``` purescript
foldGen' :: forall f a b. (Monad f) => (b -> a -> Maybe b) -> b -> GenState -> GenT f a -> f (Tuple b (GenT f a))
```

Folds over a generator to produce a value. Either the generator or the
user-defined function may halt the fold. Returns not just the value
created through folding, but also the successor generator.

#### `foldGen`

``` purescript
foldGen :: forall f a b. (Monad f) => (b -> a -> Maybe b) -> b -> GenState -> GenT f a -> f b
```

Folds over a generator to produce a value. Either the generator or the
user-defined function may halt the fold.

#### `transGen`

``` purescript
transGen :: forall f a b c. (Monad f) => (b -> a -> Tuple b (Maybe c)) -> b -> GenT f a -> GenT f c
```

Transforms one gen into another, passing along user-supplied state.
Either the generator being transformed or the transforming function may
halt the transformation.

#### `perms`

``` purescript
perms :: forall f a. (Monad f) => [a] -> GenT f [a]
```

A deterministic generator that produces all possible permutations of
the specified array.

#### `nChooseK`

``` purescript
nChooseK :: forall f a. (Monad f) => Number -> [a] -> GenT f [a]
```

A deterministic generator that produces all possible combinations of
choosing exactly k elements from the specified array.

#### `suchThat`

``` purescript
suchThat :: forall f a. (Monad f) => GenT f a -> (a -> Boolean) -> GenT f a
```

Filters a generator to produce only values satisfying the specified
predicate.

#### `suchThatMaybe`

``` purescript
suchThatMaybe :: forall f a. (Monad f) => Number -> GenT f a -> (a -> Boolean) -> GenT f (Maybe a)
```

Filters a generator to produce only values satisfying the specified
predicate, but gives up and produces Nothing after the specified number
of attempts.

#### `allInRange`

``` purescript
allInRange :: forall f a. (Monad f) => Number -> Number -> GenT f Number
```

A deterministic generator that produces integers from the specified
inclusive range, in sequence.

#### `allInArray`

``` purescript
allInArray :: forall f a. (Monad f) => [a] -> GenT f a
```

A deterministic generator that produces values from the specified array,
in sequence.

#### `collectAll`

``` purescript
collectAll :: forall f a. (Monad f) => GenState -> GenT f a -> f [a]
```

Drains a finite generator of all values. Or blows up if you called it on
an infinite generator.

#### `applyGen`

``` purescript
applyGen :: forall f a. (Monad f) => GenState -> GenT f a -> f (Maybe (GenOut (Tuple a (GenT f a))))
```

Applies a state to a generator to possibly produce the next state,
a value, and the next generator.

#### `sample'`

``` purescript
sample' :: forall f a. (Monad f) => Number -> GenState -> GenT f a -> f [a]
```

Samples a generator, producing the specified number of values.

#### `sample`

``` purescript
sample :: forall f a. (Monad f) => Number -> GenT f a -> f [a]
```

Samples a generator, producing the specified number of values. Uses
default settings for the initial generator state.

#### `showSample'`

``` purescript
showSample' :: forall r a. (Show a) => Number -> Gen a -> Eff (trace :: Trace | r) Unit
```

Shows a sample of values generated from the specified generator.

#### `showSample`

``` purescript
showSample :: forall r a. (Show a) => Gen a -> Eff (trace :: Trace | r) Unit
```

Shows a sample of values generated from the specified generator.

#### `runGen`

``` purescript
runGen :: forall f a. (Monad f) => Number -> GenState -> GenT f a -> f (Tuple [a] (GenT f a))
```

Runs a generator to produce a specified number of values, returning both
an array containing the values and the successor Gen that can be used to
continue the generation process at a later time.

#### `chunked`

``` purescript
chunked :: forall f a. (Monad f) => Number -> GenT f a -> GenT f [a]
```

Creates a generator that produces chunks of values in the specified size.
Will extend the generator if necessary to produce a chunk of the specified
size, but will not turn a finite generator into an infinite generator.

#### `wrapEffect`

``` purescript
wrapEffect :: forall f a. (Monad f) => f a -> GenT f a
```

Wraps an effect in a generator that ignores the input state.

#### `shuffle'`

``` purescript
shuffle' :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a
```

Creates a generator that mixes up the order of the specified generator.
This is achieved by chunking the generator with the specified size
and then shuffling each chunk before continuing to the next.

#### `shuffle`

``` purescript
shuffle :: forall f a. (Monad f) => GenT f a -> GenT f a
```

Same as shuffle' but with default for the chunk size.

#### `shuffleArray`

``` purescript
shuffleArray :: forall f a. (Monad f) => [a] -> GenT f [a]
```

Creates a generator that produces shuffled versions of the provided array.

#### `toLazyList`

``` purescript
toLazyList :: forall a. Gen a -> GenState -> ListT.ListT Lazy a
```

Converts the generator into a function that, given the initial state,
returns a lazy list.

#### `semigroupGenState`

``` purescript
instance semigroupGenState :: Semigroup GenState
```


#### `monoidGenState`

``` purescript
instance monoidGenState :: Monoid GenState
```


#### `semigroupGenOut`

``` purescript
instance semigroupGenOut :: (Semigroup a) => Semigroup (GenOut a)
```


#### `monoidGenOut`

``` purescript
instance monoidGenOut :: (Monoid a) => Monoid (GenOut a)
```


#### `functorGenOut`

``` purescript
instance functorGenOut :: Functor GenOut
```


#### `applyGenOut`

``` purescript
instance applyGenOut :: Apply GenOut
```


#### `functorGenT`

``` purescript
instance functorGenT :: (Monad f) => Functor (GenT f)
```

#### `applyGenT`

``` purescript
instance applyGenT :: (Monad f) => Apply (GenT f)
```


#### `applicativeGenT`

``` purescript
instance applicativeGenT :: (Monad f) => Applicative (GenT f)
```


#### `semigroupGenT`

``` purescript
instance semigroupGenT :: (Monad f) => Semigroup (GenT f a)
```


#### `monoidGenT`

``` purescript
instance monoidGenT :: (Monad f) => Monoid (GenT f a)
```


#### `bindGenT`

``` purescript
instance bindGenT :: (Monad f) => Bind (GenT f)
```


#### `monadGenT`

``` purescript
instance monadGenT :: (Monad f) => Monad (GenT f)
```


#### `altGenT`

``` purescript
instance altGenT :: (Monad f) => Alt (GenT f)
```


#### `plusGenT`

``` purescript
instance plusGenT :: (Monad f) => Plus (GenT f)
```


#### `alternativeGenT`

``` purescript
instance alternativeGenT :: (Monad f) => Alternative (GenT f)
```


#### `monadPlusGenT`

``` purescript
instance monadPlusGenT :: (Monad f) => MonadPlus (GenT f)
```



## Module Test.StrongCheck.Landscape

#### `DriverStateRec`

``` purescript
type DriverStateRec a = { state :: GenState, variance :: Number, value :: a }
```


#### `DriverState`

``` purescript
newtype DriverState a
  = DriverState (DriverStateRec a)
```


#### `Landscape`

``` purescript
newtype Landscape a
  = Landscape (Cofree L.List (DriverState a))
```


#### `Variance`

``` purescript
type Variance = Number
```


#### `Decay`

``` purescript
type Decay = Number -> Number
```


#### `decayHalf`

``` purescript
decayHalf :: Decay
```


#### `decayThird`

``` purescript
decayThird :: Decay
```


#### `defaultDecay`

``` purescript
defaultDecay :: Decay
```


#### `whereAt`

``` purescript
whereAt :: forall a. Landscape a -> a
```


#### `everywhere'`

``` purescript
everywhere' :: forall a. (Perturb a) => GenState -> Decay -> Variance -> Gen a -> L.List (Landscape a)
```

Creates a landscape whose initial points are randomly chosen across
the entire landscape.

#### `everywhere`

``` purescript
everywhere :: forall a. (Perturb a) => Variance -> Gen a -> L.List (Landscape a)
```

Creates a landscape whose initial points are randomly chosen across
the entire landscape, using the default GenState and Decay.

#### `somewhere'`

``` purescript
somewhere' :: forall a. (Perturb a) => GenState -> Decay -> Variance -> Gen a -> Maybe (Landscape a)
```

Picks somewhere and forms a landscape around that location.

#### `somewhere`

``` purescript
somewhere :: forall a. (Perturb a) => Variance -> Gen a -> Maybe (Landscape a)
```

Picks somewhere and forms a landscape around that location, using the
default GenState and Decay.

#### `nearby'`

``` purescript
nearby' :: forall a. (Perturb a) => GenState -> Decay -> a -> Variance -> Landscape a
```

Creates a landscape that samples the area around a location.

#### `nearby`

``` purescript
nearby :: forall a. (Perturb a) => a -> Variance -> Landscape a
```

Creates a landscape that samples the area around a location, using the 
default GenState and Decay.

#### `sampleHere'`

``` purescript
sampleHere' :: forall a. (Perturb a) => Number -> Landscape a -> [DriverState a]
```

Samples around the current location area, returning full state information.

#### `sampleHere`

``` purescript
sampleHere :: forall a. (Perturb a) => Number -> Landscape a -> [a]
```

Samples around the current location area, returning just the values.

#### `moveTo`

``` purescript
moveTo :: forall a. (Eq a, Perturb a) => a -> Landscape a -> Maybe (Landscape a)
```

Moves to a location in a landscape that was previously sampled.

#### `unDriverState`

``` purescript
unDriverState :: forall a. DriverState a -> DriverStateRec a
```


#### `unLandscape`

``` purescript
unLandscape :: forall a. Landscape a -> Cofree L.List (DriverState a)
```



## Module Test.StrongCheck.Perturb

#### `Attempts`

``` purescript
newtype Attempts
  = Attempts Number
```


#### `Perturber`

``` purescript
newtype Perturber a
  = Perturber (PerturberRec a)
```


#### `PerturberRec`

``` purescript
type PerturberRec a = { dims :: a -> Number, dist :: a -> a -> Number, perturb :: Number -> a -> Gen a }
```


#### `unPerturber`

``` purescript
unPerturber :: forall a. Perturber a -> PerturberRec a
```


#### `xmap`

``` purescript
xmap :: forall a b. (a -> b) -> (b -> a) -> Perturber a -> Perturber b
```

#### `Perturb`

``` purescript
class Perturb a where
  perturber :: Perturber a
```

The class for things which can be perturbed.

Laws:
  forall a, 0 >= n <= 1:
  ((>=) n) <<< dist a <$> (perturb n a) must be an infinite generator of `true` values.

#### `perturb`

``` purescript
perturb :: forall a. (Perturb a) => Number -> a -> Gen a
```


#### `dist`

``` purescript
dist :: forall a. (Perturb a) => a -> a -> Number
```


#### `dims`

``` purescript
dims :: forall a. (Perturb a) => a -> Number
```


#### `nonPerturber`

``` purescript
nonPerturber :: forall a. Perturber a
```

Creates a perturber that perturbs nothing.

#### `searchIn'`

``` purescript
searchIn' :: forall a. (Perturb a) => Attempts -> Number -> (a -> Boolean) -> a -> Gen a
```

Given one example, searches for other examples that satisfy a provided
boolean predicate.

The search operates out-to-in, in an attempt to find examples that are
as far removed from the provided example as possible. The sampling size
parameter determines how many samples to take at every level of
searching, while the attempts parameter determines how many levels.

#### `searchIn`

``` purescript
searchIn :: forall a. (Perturb a) => (a -> Boolean) -> a -> Gen a
```

The same as search', but uses defaults for attempt count and sample size.
Will search a total of 10,000 examples before giving up.

#### `(</\>)`

``` purescript
(</\>) :: forall a b. Perturber a -> Perturber b -> Perturber (Tuple a b)
```

Combines two perturbers to produce a perturber of the product

#### `(<\/>)`

``` purescript
(<\/>) :: forall a b. Perturber a -> Perturber b -> Perturber (Either a b)
```

Combines two perturbers to produce a perturber of the sum

#### `bounded`

``` purescript
bounded :: Number -> Number -> Perturber Number
```

Creates a perturber for numbers that fall within the specified range.

#### `boundedInt`

``` purescript
boundedInt :: Number -> Number -> Perturber Number
```

Creates a perturber for integers that fall within the specified range.

#### `enumerated`

``` purescript
enumerated :: forall a. (Eq a) => a -> [a] -> Perturber a
```


#### `perturbArbEnum`

``` purescript
instance perturbArbEnum :: (Enum a) => Perturb (ArbEnum a)
```


#### `perturbNumber`

``` purescript
instance perturbNumber :: Perturb Number
```


#### `perturbArray`

``` purescript
instance perturbArray :: (Perturb a) => Perturb [a]
```


#### `perturbChar`

``` purescript
instance perturbChar :: Perturb Char
```


#### `perturbBoolean`

``` purescript
instance perturbBoolean :: Perturb Boolean
```


#### `perturbString`

``` purescript
instance perturbString :: Perturb String
```





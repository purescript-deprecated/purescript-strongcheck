## Module Test.StrongCheck.Gen

#### `Size`

``` purescript
type Size = Int
```

#### `Seed`

``` purescript
type Seed = Number
```

#### `GenState`

``` purescript
data GenState
  = GenState { seed :: Seed, size :: Size }
```

##### Instances
``` purescript
instance semigroupGenState :: Semigroup GenState
instance monoidGenState :: Monoid GenState
```

#### `unGenState`

``` purescript
unGenState :: GenState -> { seed :: Seed, size :: Size }
```

#### `GenOut`

``` purescript
data GenOut a
  = GenOut { state :: GenState, value :: a }
```

##### Instances
``` purescript
instance semigroupGenOut :: (Semigroup a) => Semigroup (GenOut a)
instance monoidGenOut :: (Monoid a) => Monoid (GenOut a)
instance functorGenOut :: Functor GenOut
instance applyGenOut :: Apply GenOut
```

#### `unGenOut`

``` purescript
unGenOut :: forall a. GenOut a -> { state :: GenState, value :: a }
```

#### `GenT`

``` purescript
data GenT f a
  = GenT (MealyT f GenState (GenOut a))
```

##### Instances
``` purescript
instance functorGenT :: (Monad f) => Functor (GenT f)
instance applyGenT :: (Monad f) => Apply (GenT f)
instance applicativeGenT :: (Monad f) => Applicative (GenT f)
instance semigroupGenT :: (Monad f) => Semigroup (GenT f a)
instance monoidGenT :: (Monad f) => Monoid (GenT f a)
instance bindGenT :: (Monad f) => Bind (GenT f)
instance monadGenT :: (Monad f) => Monad (GenT f)
instance altGenT :: (Monad f) => Alt (GenT f)
instance plusGenT :: (Monad f) => Plus (GenT f)
instance alternativeGenT :: (Monad f) => Alternative (GenT f)
instance monadPlusGenT :: (Monad f) => MonadPlus (GenT f)
```

#### `Gen`

``` purescript
type Gen a = GenT Trampoline a
```

#### `unGen`

``` purescript
unGen :: forall f a. GenT f a -> MealyT f GenState (GenOut a)
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
chooseInt :: forall f. (Monad f) => Number -> Number -> GenT f Int
```

Creates a generator that generates integers between the specified
inclusive range.

#### `oneOf`

``` purescript
oneOf :: forall f a. (Monad f) => GenT f a -> Array (GenT f a) -> GenT f a
```

Creates a generator that chooses another generator from the specified list
at random, and then generates a value with that generator.

#### `frequency`

``` purescript
frequency :: forall f a. (Monad f) => Tuple Number (GenT f a) -> List (Tuple Number (GenT f a)) -> GenT f a
```

Generates elements by the specified frequencies (which will be normalized).

#### `arrayOf`

``` purescript
arrayOf :: forall f a. (Monad f) => GenT f a -> GenT f (Array a)
```

Creates a generator of elements ranging from 0 to the maximum size.

#### `arrayOf1`

``` purescript
arrayOf1 :: forall f a. (Monad f) => GenT f a -> GenT f (Tuple a (Array a))
```

Creates a generator of elements ranging from 1 to the maximum size.

#### `vectorOf`

``` purescript
vectorOf :: forall f a. (Monad f) => Int -> GenT f a -> GenT f (Array a)
```

Creates a generator that generates arrays of some specified size.

#### `elements`

``` purescript
elements :: forall f a. (Monad f) => a -> List a -> GenT f a
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
takeGen :: forall f a. (Monad f) => Int -> GenT f a -> GenT f a
```

Takes the first number of values from the generator. Will turn an infinite
generator into a finite generator.

#### `dropGen`

``` purescript
dropGen :: forall f a. (Monad f) => Int -> GenT f a -> GenT f a
```

Drops a certain number of values from the generator. May produce
an empty generator if called on a finite generator.

#### `extend`

``` purescript
extend :: forall f a. (Monad f) => Int -> GenT f a -> GenT f a
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
perms :: forall f a. (Monad f) => Array a -> GenT f (Array a)
```

A deterministic generator that produces all possible permutations of
the specified array.

#### `nChooseK`

``` purescript
nChooseK :: forall f a. (Monad f) => Int -> Array a -> GenT f (Array a)
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
suchThatMaybe :: forall f a. (Monad f) => GenT f a -> (a -> Boolean) -> GenT f (Maybe a)
```

Tries to filter a generator such that it only produces values satisfying the
specified predicate.

#### `allInRange`

``` purescript
allInRange :: forall f a. (Monad f) => Int -> Int -> GenT f Int
```

A deterministic generator that produces integers from the specified
inclusive range, in sequence.

#### `allInArray`

``` purescript
allInArray :: forall f a. (Monad f) => Array a -> GenT f a
```

A deterministic generator that produces values from the specified array,
in sequence.

#### `collectAll`

``` purescript
collectAll :: forall f a. (Monad f) => GenState -> GenT f a -> f (Array a)
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
sample' :: forall f a. (Monad f) => Int -> GenState -> GenT f a -> f (Array a)
```

Samples a generator, producing the specified number of values.

#### `sample`

``` purescript
sample :: forall f a. (Monad f) => Int -> GenT f a -> f (Array a)
```

Samples a generator, producing the specified number of values. Uses
default settings for the initial generator state.

#### `showSample'`

``` purescript
showSample' :: forall r a. (Show a) => Int -> Gen a -> Eff (console :: CONSOLE | r) Unit
```

Shows a sample of values generated from the specified generator.

#### `showSample`

``` purescript
showSample :: forall r a. (Show a) => Gen a -> Eff (console :: CONSOLE | r) Unit
```

Shows a sample of values generated from the specified generator.

#### `runGen`

``` purescript
runGen :: forall f a. (Monad f) => Int -> GenState -> GenT f a -> f (Tuple (Array a) (GenT f a))
```

Runs a generator to produce a specified number of values, returning both
an array containing the values and the successor Gen that can be used to
continue the generation process at a later time.

#### `chunked`

``` purescript
chunked :: forall f a. (Monad f) => Int -> GenT f a -> GenT f (Array a)
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
shuffle' :: forall f a. (Monad f) => Int -> GenT f a -> GenT f a
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
shuffleArray :: forall f a. (Monad f) => Array a -> GenT f (Array a)
```

Creates a generator that produces shuffled versions of the provided array.

#### `toLazyList`

``` purescript
toLazyList :: forall a. Gen a -> GenState -> ListT Lazy a
```

Converts the generator into a function that, given the initial state,
returns a lazy list.



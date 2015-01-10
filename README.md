# Module Documentation

## Module Test.StrongCheck

### Types


    newtype AlphaNumString where
      AlphaNumString :: String -> AlphaNumString


    newtype ArbEnum a where
      ArbEnum :: a -> ArbEnum a


    newtype Negative where
      Negative :: Number -> Negative


    newtype NonZero where
      NonZero :: Number -> NonZero


    newtype Positive where
      Positive :: Number -> Positive


    type QC a = forall eff. Eff (err :: Exception, random :: Random, trace :: Trace | eff) a


    data Result where
      Success :: Result
      Failed :: String -> Result


    newtype Signum where
      Signum :: Number -> Signum


### Type Classes


    class Arbitrary t where

      arbitrary :: Gen t


    class CoArbitrary t where

      coarbitrary :: forall r. t -> Gen r -> Gen r


    class Testable prop where

      test :: prop -> Gen Result


### Type Class Instances


    instance arbAlphaNumString :: Arbitrary AlphaNumString


    instance arbArbEnum :: (Enum a) => Arbitrary (ArbEnum a)


    instance arbArray :: (Arbitrary a) => Arbitrary [a]


    instance arbBoolean :: Arbitrary Boolean


    instance arbChar :: Arbitrary Char


    instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)


    instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)


    instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a)


    instance arbNegative :: Arbitrary Negative


    instance arbNonZero :: Arbitrary NonZero


    instance arbNumber :: Arbitrary Number


    instance arbPositive :: Arbitrary Positive


    instance arbSignum :: Arbitrary Signum


    instance arbString :: Arbitrary String


    instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)


    instance coarbAlphaNumString :: CoArbitrary AlphaNumString


    instance coarbArbEnum :: (Enum a) => CoArbitrary (ArbEnum a)


    instance coarbArray :: (CoArbitrary a) => CoArbitrary [a]


    instance coarbBoolean :: CoArbitrary Boolean


    instance coarbChar :: CoArbitrary Char


    instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b)


    instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)


    instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a)


    instance coarbNegative :: CoArbitrary Negative


    instance coarbNonZero :: CoArbitrary NonZero


    instance coarbNumber :: CoArbitrary Number


    instance coarbPositive :: CoArbitrary Positive


    instance coarbSignum :: CoArbitrary Signum


    instance coarbString :: CoArbitrary String


    instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b)


    instance enumArbEnum :: (Enum a) => Enum (ArbEnum a)


    instance eqArbEnum :: (Eq a) => Eq (ArbEnum a)


    instance eqResult :: Eq Result


    instance monoidResult :: Monoid Result


    instance ordArbEnum :: (Ord a) => Ord (ArbEnum a)


    instance semigroupResult :: Semigroup Result


    instance showArbEnum :: (Show a) => Show (ArbEnum a)


    instance showResult :: Show Result


    instance testableBoolean :: Testable Boolean


    instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop)


    instance testableResult :: Testable Result


### Values


    (/==) :: forall a b. (Eq a, Show a) => a -> a -> Result


    (<?>) :: Boolean -> String -> Result


    (===) :: forall a b. (Eq a, Show a) => a -> a -> Result

     | Checks that the specified proposition holds. Useful for unit tests.

    assert :: forall prop. (Testable prop) => prop -> QC Unit

     | Checks the proposition for 100 random values.

    quickCheck :: forall prop. (Testable prop) => prop -> QC Unit


    quickCheck' :: forall prop. (Testable prop) => Number -> prop -> QC Unit


    quickCheckPure :: forall prop. (Testable prop) => Number -> Seed -> prop -> [Result]


    runArbEnum :: forall a. ArbEnum a -> a

     | Exhaustively checks the proposition for all possible values. Assumes the
     | generator is a finite generator.

    smallCheck :: forall prop. (Testable prop) => prop -> QC Unit


    smallCheckPure :: forall prop. (Testable prop) => Number -> prop -> [Result]

     | Checks that the proposition has a certain probability of being true for 
     | arbitrary values.

    statCheck :: forall prop. (Testable prop) => Number -> prop -> QC Unit


    statCheckPure :: forall prop. (Testable prop) => Seed -> Number -> prop -> Result


## Module Test.StrongCheck.Gen

### Types


    type Gen a = GenT Trampoline a


    data GenOut a where
      GenOut :: { value :: a, state :: GenState } -> GenOut a


    data GenState where
      GenState :: { size :: Size, seed :: Seed } -> GenState


    data GenT f a where
      GenT :: Mealy.MealyT f GenState (GenOut a) -> GenT f a


    type Seed = Number


    type Size = Number


### Type Class Instances


    instance altGenT :: (Monad f) => Alt (GenT f)


    instance alternativeGenT :: (Monad f) => Alternative (GenT f)


    instance applicativeGenT :: (Monad f) => Applicative (GenT f)


    instance applyGenOut :: Apply GenOut


    instance applyGenT :: (Monad f) => Apply (GenT f)


    instance bindGenT :: (Monad f) => Bind (GenT f)


    instance functorGenOut :: Functor GenOut

     GenT instances

    instance functorGenT :: (Monad f) => Functor (GenT f)


    instance monadGenT :: (Monad f) => Monad (GenT f)


    instance monadPlusGenT :: (Monad f) => MonadPlus (GenT f)


    instance monoidGenOut :: (Monoid a) => Monoid (GenOut a)


    instance monoidGenState :: Monoid GenState


    instance monoidGenT :: (Monad f) => Monoid (GenT f a)


    instance plusGenT :: (Monad f) => Plus (GenT f)


    instance semigroupGenOut :: (Semigroup a) => Semigroup (GenOut a)


    instance semigroupGenState :: Semigroup GenState


    instance semigroupGenT :: (Monad f) => Semigroup (GenT f a)


### Values

     | A deterministic generator that produces values from the specified array,
     | in sequence.

    allInArray :: forall f a. (Monad f) => [a] -> GenT f a

     | A deterministic generator that produces integers from the specified 
     | inclusive range, in sequence.

    allInRange :: forall f a. (Monad f) => Number -> Number -> GenT f Number

     | Applies a state to a generator to possibly produce the next state,
     | a value, and the next generator.

    applyGen :: forall f a. (Monad f) => GenState -> GenT f a -> f (Maybe (GenOut (Tuple a (GenT f a))))

     | Creates a generator of elements ranging from 0 to the maximum size.

    arrayOf :: forall f a. (Monad f) => GenT f a -> GenT f [a]

     | Creates a generator of elements ranging from 1 to the maximum size.

    arrayOf1 :: forall f a. (Monad f) => GenT f a -> GenT f (Tuple a [a])

     | A generator for characters.

    charGen :: forall f. (Monad f) => GenT f Char

     | Creates a generator that generates real numbers between the specified
     | inclusive range.

    choose :: forall f. (Monad f) => Number -> Number -> GenT f Number

     | Creates a generator that generates integers between the specified 
     | inclusive range.

    chooseInt :: forall f. (Monad f) => Number -> Number -> GenT f Number

     | Creates a generator that produces chunks of values in the specified size.
     | Will extend the generator if necessary to produce a chunk of the specified
     | size, but will not turn a finite generator into an infinite generator.

    chunked :: forall f a. (Monad f) => Number -> GenT f a -> GenT f [a]

     | Drains a finite generator of all values. Or blows up if you called it on 
     | an infinite generator.

    collectAll :: forall f a. (Monad f) => GenState -> GenT f a -> f [a]

     | Drops a certain number of values from the generator. May produce
     | an empty generator if called on a finite generator.

    dropGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

     | Creates a generator that chooses an element from among a set of elements.

    elements :: forall f a. (Monad f) => a -> [a] -> GenT f a

     | Extends a generator to produce *at least* the specified number of values.
     | Will not turn a finite generator into an infinite one.

    extend :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

     | Folds over a generator to produce a value. Either the generator or the 
     | user-defined function may halt the fold.

    foldGen :: forall f a b. (Monad f) => (b -> a -> Maybe b) -> b -> GenState -> GenT f a -> f b

     | Folds over a generator to produce a value. Either the generator or the 
     | user-defined function may halt the fold. Returns not just the value
     | created through folding, but also the successor generator.

    foldGen' :: forall f a b. (Monad f) => (b -> a -> Maybe b) -> b -> GenState -> GenT f a -> f (Tuple b (GenT f a))

     | Generates elements by the specified frequencies (which will be normalized).

    frequency :: forall f a. (Monad f) => Tuple Number (GenT f a) -> [Tuple Number (GenT f a)] -> GenT f a

     | Ensures that a given generator can produce an infinite number of values,
     | assuming it can produce at least one.

    infinite :: forall f a. (Monad f) => GenT f a -> GenT f a

     | Fairly interleaves two generators.

    interleave :: forall f a. (Monad f) => GenT f a -> GenT f a -> GenT f a

     | A deterministic generator that produces all possible combinations of
     | choosing exactly k elements from the specified array.

    nChooseK :: forall f a. (Monad f) => Number -> [a] -> GenT f [a]

     | Creates a generator that chooses another generator from the specified list
     | at random, and then generates a value with that generator.

    oneOf :: forall f a. (Monad f) => GenT f a -> [GenT f a] -> GenT f a

     | A deterministic generator that produces all possible permutations of 
     | the specified array.

    perms :: forall f a. (Monad f) => [a] -> GenT f [a]


    perturbGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

     | Creates a function generator that will always generate the same output 
     | for the same input.

    repeatable :: forall a b. (a -> Gen b) -> Gen (a -> b)

     | Resizes the generator so the size parameter passed into the generator 
     | will be equal to the specified size.

    resize :: forall f a. (Monad f) => Size -> GenT f a -> GenT f a

     | Runs a generator to produce a specified number of values, returning both
     | an array containing the values and the successor Gen that can be used to
     | continue the generation process at a later time.

    runGen :: forall f a. (Monad f) => Number -> GenState -> GenT f a -> f (Tuple [a] (GenT f a))

     | Samples a generator, producing the specified number of values. Uses 
     | default settings for the initial generator state.

    sample :: forall f a. (Monad f) => Number -> GenT f a -> f [a]

     | Samples a generator, producing the specified number of values.

    sample' :: forall f a. (Monad f) => Number -> GenState -> GenT f a -> f [a]

     | Shows a sample of values generated from the specified generator.

    showSample :: forall r a. (Show a) => Gen a -> Eff (trace :: Trace | r) Unit

     | Shows a sample of values generated from the specified generator.

    showSample' :: forall r a. (Show a) => Number -> Gen a -> Eff (trace :: Trace | r) Unit

     | Same as shuffle' but with default for the chunk size.

    shuffle :: forall f a. (Monad f) => GenT f a -> GenT f a

     | Creates a generator that mixes up the order of the specified generator.
     | This is achieved by chunking the generator with the specified size 
     | and then shuffling each chunk before continuing to the next.

    shuffle' :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

     | Creates a generator that produces shuffled versions of the provided array.

    shuffleArray :: forall f a. (Monad f) => [a] -> GenT f [a]

     | Creates a generator that depends on the size parameter.

    sized :: forall f a. (Monad f) => (Size -> GenT f a) -> GenT f a

     | Creates a generator that depends on access to the generator state.

    stateful :: forall f a. (Monad f) => (GenState -> GenT f a) -> GenT f a

     | Filters a generator to produce only values satisfying the specified 
     | predicate.

    suchThat :: forall f a. (Monad f) => GenT f a -> (a -> Boolean) -> GenT f a

     | Filters a generator to produce only values satisfying the specified 
     | predicate, but gives up and produces Nothing after the specified number
     | of attempts.

    suchThatMaybe :: forall f a. (Monad f) => Number -> GenT f a -> (a -> Boolean) -> GenT f (Maybe a)

     | Takes the first number of values from the generator. Will turn an infinite
     | generator into a finite generator.

    takeGen :: forall f a. (Monad f) => Number -> GenT f a -> GenT f a

     | Converts the generator into a function that, given the initial state, 
     | returns a lazy list.

    toLazyList :: forall a. Gen a -> GenState -> ListT.ListT Lazy a

     | Transforms one gen into another, passing along user-supplied state.
     | Either the generator being transformed or the transforming function may
     | halt the transformation.

    transGen :: forall f a b c. (Monad f) => (b -> a -> Tuple b (Maybe c)) -> b -> GenT f a -> GenT f c


    unGen :: forall f a. GenT f a -> Mealy.MealyT f GenState (GenOut a)


    unGenOut :: forall a. GenOut a -> { value :: a, state :: GenState }


    unGenState :: GenState -> { size :: Size, seed :: Seed }


    uniform :: forall f. (Monad f) => GenT f Seed


    updateSeedState :: GenState -> GenState

     | Fixes a generator on a certain variant, given by the specified seed.

    variant :: forall f a. (Monad f) => Seed -> GenT f a -> GenT f a

     | Creates a generator that generates arrays of some specified size.

    vectorOf :: forall f a. (Monad f) => Number -> GenT f a -> GenT f [a]

     | Wraps an effect in a generator that ignores the input state.

    wrapEffect :: forall f a. (Monad f) => f a -> GenT f a


## Module Test.StrongCheck.Landscape

### Types


    type Decay = Number -> Number


    newtype DriverState a where
      DriverState :: DriverStateRec a -> DriverState a


    type DriverStateRec a = { state :: GenState, variance :: Number, value :: a }


    newtype Landscape a where
      Landscape :: Cofree L.List (DriverState a) -> Landscape a


    type Variance = Number


### Values


    decayHalf :: Decay


    decayThird :: Decay


    defaultDecay :: Decay

     | Creates a landscape whose initial points are randomly chosen across
     | the entire landscape, using the default GenState and Decay.

    everywhere :: forall a. (Perturb a) => Variance -> Gen a -> L.List (Landscape a)

     | Creates a landscape whose initial points are randomly chosen across
     | the entire landscape.

    everywhere' :: forall a. (Perturb a) => GenState -> Decay -> Variance -> Gen a -> L.List (Landscape a)

     | Moves to a location in a landscape that was previously sampled.

    moveTo :: forall a. (Eq a, Perturb a) => a -> Landscape a -> Maybe (Landscape a)

     | Creates a landscape that samples the area around a location, using the 
     | default GenState and Decay.

    nearby :: forall a. (Perturb a) => a -> Variance -> Landscape a

     | Creates a landscape that samples the area around a location.

    nearby' :: forall a. (Perturb a) => GenState -> Decay -> a -> Variance -> Landscape a

     | Samples around the current location area, returning just the values.

    sampleHere :: forall a. (Perturb a) => Number -> Landscape a -> [a]

     | Samples around the current location area, returning full state information.

    sampleHere' :: forall a. (Perturb a) => Number -> Landscape a -> [DriverState a]

     | Picks somewhere and forms a landscape around that location, using the
     | default GenState and Decay.

    somewhere :: forall a. (Perturb a) => Variance -> Gen a -> Maybe (Landscape a)

     | Picks somewhere and forms a landscape around that location.

    somewhere' :: forall a. (Perturb a) => GenState -> Decay -> Variance -> Gen a -> Maybe (Landscape a)


    unDriverState :: forall a. DriverState a -> DriverStateRec a


    unLandscape :: forall a. Landscape a -> Cofree L.List (DriverState a)


    whereAt :: forall a. Landscape a -> a


## Module Test.StrongCheck.Perturb

### Types


    newtype Attempts where
      Attempts :: Number -> Attempts


    newtype Perturber a where
      Perturber :: PerturberRec a -> Perturber a


    type PerturberRec a = { dims :: a -> Number, dist :: a -> a -> Number, perturb :: Number -> a -> Gen a }


### Type Classes

     | The class for things which can be perturbed.
     |
     | Laws:  
     |   forall a, 0 >= n <= 1:  
     |   ((>=) n) <<< dist a <$> (perturb n a) must be an infinite generator of `true` values.

    class Perturb a where

      perturber :: Perturber a


### Type Class Instances


    instance perturbArbEnum :: (Enum a) => Perturb (ArbEnum a)


    instance perturbArray :: (Perturb a) => Perturb [a]


    instance perturbBoolean :: Perturb Boolean


    instance perturbChar :: Perturb Char


    instance perturbNumber :: Perturb Number


    instance perturbString :: Perturb String


### Values

     | Combines two perturbers to produce a perturber of the product

    (</\>) :: forall a b. Perturber a -> Perturber b -> Perturber (Tuple a b)

     | Combines two perturbers to produce a perturber of the sum

    (<\/>) :: forall a b. Perturber a -> Perturber b -> Perturber (Either a b)

     | Creates a perturber for numbers that fall within the specified range.

    bounded :: Number -> Number -> Perturber Number

     | Creates a perturber for integers that fall within the specified range.

    boundedInt :: Number -> Number -> Perturber Number


    dims :: forall a. (Perturb a) => a -> Number


    dist :: forall a. (Perturb a) => a -> a -> Number


    enumerated :: forall a. (Eq a) => a -> [a] -> Perturber a

     | Creates a perturber that perturbs nothing.

    nonPerturber :: forall a. Perturber a


    perturb :: forall a. (Perturb a) => Number -> a -> Gen a

     | The same as search', but uses defaults for attempt count and sample size.
     | Will search a total of 10,000 examples before giving up.

    searchIn :: forall a. (Perturb a) => (a -> Boolean) -> a -> Gen a

     | Given one example, searches for other examples that satisfy a provided
     | boolean predicate.
     | 
     | The search operates out-to-in, in an attempt to find examples that are 
     | as far removed from the provided example as possible. The sampling size
     | parameter determines how many samples to take at every level of 
     | searching, while the attempts parameter determines how many levels.

    searchIn' :: forall a. (Perturb a) => Attempts -> Number -> (a -> Boolean) -> a -> Gen a


    unPerturber :: forall a. Perturber a -> PerturberRec a

     TODO: Move to Data.Functor.Invariant

    xmap :: forall a b. (a -> b) -> (b -> a) -> Perturber a -> Perturber b




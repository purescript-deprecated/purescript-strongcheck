## Module Test.StrongCheck

#### `Arbitrary`

``` purescript
class Arbitrary t where
  arbitrary :: Gen t
```

##### Instances
``` purescript
instance arbNumber :: Arbitrary Number
instance arbInt :: Arbitrary Int
instance arbPositive :: Arbitrary Positive
instance arbNegative :: Arbitrary Negative
instance arbNonZero :: Arbitrary NonZero
instance arbSignum :: Arbitrary Signum
instance arbArbEnum :: (Enum a) => Arbitrary (ArbEnum a)
instance arbBoolean :: Arbitrary Boolean
instance arbChar :: Arbitrary Char
instance arbString :: Arbitrary String
instance arbAlphaNumString :: Arbitrary AlphaNumString
instance arbTuple :: (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b)
instance arbEither :: (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
instance arbMaybe :: (Arbitrary a) => Arbitrary (Maybe a)
instance arbFunction :: (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)
instance arbArray :: (Arbitrary a) => Arbitrary (Array a)
```

#### `CoArbitrary`

``` purescript
class CoArbitrary t where
  coarbitrary :: forall r. t -> Gen r -> Gen r
```

##### Instances
``` purescript
instance coarbNumber :: CoArbitrary Number
instance coarbInt :: CoArbitrary Int
instance coarbPositive :: CoArbitrary Positive
instance coarbNegative :: CoArbitrary Negative
instance coarbNonZero :: CoArbitrary NonZero
instance coarbSignum :: CoArbitrary Signum
instance coarbArbEnum :: (Enum a) => CoArbitrary (ArbEnum a)
instance coarbBoolean :: CoArbitrary Boolean
instance coarbChar :: CoArbitrary Char
instance coarbString :: CoArbitrary String
instance coarbAlphaNumString :: CoArbitrary AlphaNumString
instance coarbTuple :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Tuple a b)
instance coarbEither :: (CoArbitrary a, CoArbitrary b) => CoArbitrary (Either a b)
instance coarbMaybe :: (CoArbitrary a) => CoArbitrary (Maybe a)
instance coarbFunction :: (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b)
instance coarbArray :: (CoArbitrary a) => CoArbitrary (Array a)
```

#### `Testable`

``` purescript
class Testable prop where
  test :: prop -> Gen Result
```

##### Instances
``` purescript
instance testableResult :: Testable Result
instance testableBoolean :: Testable Boolean
instance testableFunction :: (Arbitrary t, Testable prop) => Testable (t -> prop)
```

#### `AlphaNumString`

``` purescript
newtype AlphaNumString
  = AlphaNumString String
```

##### Instances
``` purescript
instance arbAlphaNumString :: Arbitrary AlphaNumString
instance coarbAlphaNumString :: CoArbitrary AlphaNumString
```

#### `Positive`

``` purescript
newtype Positive
  = Positive Number
```

##### Instances
``` purescript
instance arbPositive :: Arbitrary Positive
instance coarbPositive :: CoArbitrary Positive
```

#### `Negative`

``` purescript
newtype Negative
  = Negative Number
```

##### Instances
``` purescript
instance arbNegative :: Arbitrary Negative
instance coarbNegative :: CoArbitrary Negative
```

#### `NonZero`

``` purescript
newtype NonZero
  = NonZero Number
```

##### Instances
``` purescript
instance arbNonZero :: Arbitrary NonZero
instance coarbNonZero :: CoArbitrary NonZero
```

#### `Signum`

``` purescript
newtype Signum
  = Signum Int
```

##### Instances
``` purescript
instance arbSignum :: Arbitrary Signum
instance coarbSignum :: CoArbitrary Signum
```

#### `ArbEnum`

``` purescript
newtype ArbEnum a
  = ArbEnum a
```

##### Instances
``` purescript
instance arbArbEnum :: (Enum a) => Arbitrary (ArbEnum a)
instance coarbArbEnum :: (Enum a) => CoArbitrary (ArbEnum a)
instance eqArbEnum :: (Eq a) => Eq (ArbEnum a)
instance ordArbEnum :: (Ord a) => Ord (ArbEnum a)
instance showArbEnum :: (Show a) => Show (ArbEnum a)
instance boundedArbEnum :: (Bounded a) => Bounded (ArbEnum a)
instance enumArbEnum :: (Enum a) => Enum (ArbEnum a)
```

#### `QC`

``` purescript
type QC a = forall eff. Eff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) a
```

#### `Result`

``` purescript
data Result
  = Success
  | Failed String
```

##### Instances
``` purescript
instance eqResult :: Eq Result
instance showResult :: Show Result
instance semigroupResult :: Semigroup Result
instance monoidResult :: Monoid Result
instance testableResult :: Testable Result
```

#### `(<?>)`

``` purescript
(<?>) :: Boolean -> String -> Result
```

_left-associative / precedence -1_

#### `(===)`

``` purescript
(===) :: forall a. (Eq a, Show a) => a -> a -> Result
```

_left-associative / precedence -1_

#### `(/==)`

``` purescript
(/==) :: forall a. (Eq a, Show a) => a -> a -> Result
```

_left-associative / precedence -1_

#### `quickCheckPure`

``` purescript
quickCheckPure :: forall prop. (Testable prop) => Int -> Seed -> prop -> Array Result
```

#### `quickCheck'`

``` purescript
quickCheck' :: forall prop. (Testable prop) => Int -> prop -> QC Unit
```

#### `quickCheck`

``` purescript
quickCheck :: forall prop. (Testable prop) => prop -> QC Unit
```

Checks the proposition for 100 random values.

#### `smallCheckPure`

``` purescript
smallCheckPure :: forall prop. (Testable prop) => Seed -> prop -> Array Result
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

#### `runAlphaNumString`

``` purescript
runAlphaNumString :: AlphaNumString -> String
```

#### `runSignum`

``` purescript
runSignum :: Signum -> Int
```

#### `runPositive`

``` purescript
runPositive :: Positive -> Number
```

#### `runNegative`

``` purescript
runNegative :: Negative -> Number
```

#### `runNonZero`

``` purescript
runNonZero :: NonZero -> Number
```

#### `runArbEnum`

``` purescript
runArbEnum :: forall a. ArbEnum a -> a
```



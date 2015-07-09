## Module Test.StrongCheck.Perturb

#### `Attempts`

``` purescript
newtype Attempts
  = Attempts Int
```

#### `Perturber`

``` purescript
newtype Perturber a
  = Perturber (PerturberRec a)
```

##### Instances
``` purescript
instance perturberInvariant :: Invariant Perturber
```

#### `PerturberRec`

``` purescript
type PerturberRec a = { perturb :: Number -> a -> Gen a, dist :: a -> a -> Number, dims :: a -> Number }
```

#### `unPerturber`

``` purescript
unPerturber :: forall a. Perturber a -> PerturberRec a
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

##### Instances
``` purescript
instance perturbArbEnum :: (Enum a, Eq a) => Perturb (ArbEnum a)
instance perturbNumber :: Perturb Number
instance perturbInt :: Perturb Int
instance perturbList :: (Perturb a) => Perturb (List a)
instance perturbChar :: Perturb Char
instance perturbBoolean :: Perturb Boolean
instance perturbString :: Perturb String
```

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
searchIn' :: forall a. (Perturb a) => Attempts -> Int -> (a -> Boolean) -> a -> Gen a
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

_right-associative / precedence 6_

Combines two perturbers to produce a perturber of the product

#### `(<\/>)`

``` purescript
(<\/>) :: forall a b. Perturber a -> Perturber b -> Perturber (Either a b)
```

_right-associative / precedence 6_

Combines two perturbers to produce a perturber of the sum

#### `bounded`

``` purescript
bounded :: Number -> Number -> Perturber Number
```

Creates a perturber for numbers that fall within the specified range.

#### `boundedInt`

``` purescript
boundedInt :: Int -> Int -> Perturber Int
```

Creates a perturber for integers that fall within the specified range.

#### `enumerated`

``` purescript
enumerated :: forall a. (Eq a) => a -> Array a -> Perturber a
```



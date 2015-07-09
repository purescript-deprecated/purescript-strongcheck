## Module Test.StrongCheck.Landscape

#### `DriverStateRec`

``` purescript
type DriverStateRec a = { value :: a, variance :: Number, state :: GenState }
```

#### `LList`

``` purescript
type LList = ListT Lazy
```

#### `DriverState`

``` purescript
newtype DriverState a
  = DriverState (DriverStateRec a)
```

#### `Landscape`

``` purescript
newtype Landscape a
  = Landscape (Cofree LList (DriverState a))
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
everywhere' :: forall a. (Perturb a) => GenState -> Decay -> Variance -> Gen a -> LList (Landscape a)
```

Creates a landscape whose initial points are randomly chosen across
the entire landscape.

#### `everywhere`

``` purescript
everywhere :: forall a. (Perturb a) => Variance -> Gen a -> LList (Landscape a)
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
sampleHere' :: forall a. (Perturb a) => Int -> Landscape a -> Array (DriverState a)
```

Samples around the current location area, returning full state information.

#### `sampleHere`

``` purescript
sampleHere :: forall a. (Perturb a) => Int -> Landscape a -> Array a
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
unLandscape :: forall a. Landscape a -> Cofree LList (DriverState a)
```



module Test.StrongCheck.Landscape
  ( Decay(..)
  , DriverState(..)
  , DriverStateRec(..)
  , Variance(..)
  , Landscape(..)
  , decayHalf
  , decayThird
  , defaultDecay
  , everywhere
  , everywhere'
  , moveTo
  , nearby
  , nearby'
  , sampleHere
  , sampleHere'
  , somewhere
  , somewhere'
  , unDriverState
  , unLandscape
  , whereAt
  ) where

  import Control.Comonad.Cofree (Cofree(), mkCofree, head, tail)
  import Control.Monad.Trampoline (runTrampoline)
  import Data.Int (Int())
  import Data.Lazy (Lazy(), force, defer)
  import Data.Maybe (Maybe(), maybe)
  import Data.Monoid (mempty)
  import Data.Tuple (fst, snd)
  import Test.StrongCheck.Gen
  import Test.StrongCheck.Perturb
  import qualified Data.List.Lazy as L

  type DriverStateRec a = { value :: a, variance :: Number, state :: GenState }

  newtype DriverState a = DriverState (DriverStateRec a)
  newtype Landscape a = Landscape (Cofree L.List (DriverState a))

  type Variance = Number
  type Decay = Number -> Number

  decayHalf :: Decay
  decayHalf v = v / 2

  decayThird :: Decay
  decayThird v = v / 3

  defaultDecay :: Decay
  defaultDecay = decayHalf

  whereAt :: forall a. Landscape a -> a
  whereAt (Landscape v) = (unDriverState (head v)).value

  -- | Creates a landscape whose initial points are randomly chosen across
  -- | the entire landscape.
  everywhere' :: forall a. (Perturb a) => GenState -> Decay -> Variance -> Gen a -> L.List (Landscape a)
  everywhere' s d v g = L.wrapEffect (go (infinite g) s)
    where go g s = defer \_ ->
                      let o = unGenOut <$> runTrampoline (applyGen s g)
                      in  maybe L.nil
                            (\o ->  let a  = fst o.value
                                        g  = snd o.value
                                        s' = o.state
                                    in  L.prepend' (nearby' s' d a v) (go g s')) o

  -- | Creates a landscape whose initial points are randomly chosen across
  -- | the entire landscape, using the default GenState and Decay.
  everywhere :: forall a. (Perturb a) => Variance -> Gen a -> L.List (Landscape a)
  everywhere = everywhere' mempty decayHalf

  -- | Picks somewhere and forms a landscape around that location.
  somewhere' :: forall a. (Perturb a) => GenState -> Decay -> Variance -> Gen a -> Maybe (Landscape a)
  somewhere' s d v = force <<< L.head <<< everywhere' s d v

  -- | Picks somewhere and forms a landscape around that location, using the
  -- | default GenState and Decay.
  somewhere :: forall a. (Perturb a) => Variance -> Gen a -> Maybe (Landscape a)
  somewhere = somewhere' mempty defaultDecay

  -- | Creates a landscape that samples the area around a location.
  nearby' :: forall a. (Perturb a) => GenState -> Decay -> a -> Variance -> Landscape a
  nearby' s d a v = Landscape $ mkCofree (mkState a v s) (loop a s v)
    where loop a s v =
            do  a' <- toLazyList (infinite (perturb v a)) s
                let h = mkState a' v s
                let t = loop a' (updateSeedState s) (d v)
                return $ mkCofree h t

  -- | Creates a landscape that samples the area around a location, using the
  -- | default GenState and Decay.
  nearby :: forall a. (Perturb a) => a -> Variance -> Landscape a
  nearby = nearby' mempty defaultDecay

  -- | Samples around the current location area, returning full state information.
  sampleHere' :: forall a. (Perturb a) => Int -> Landscape a -> [DriverState a]
  sampleHere' n = force <<< L.toArray <<< L.take n <<< (<$>) head <<< tail <<< unLandscape

  -- | Samples around the current location area, returning just the values.
  sampleHere :: forall a. (Perturb a) => Int -> Landscape a -> [a]
  sampleHere n = (<$>) (unDriverState >>> \v -> v.value) <<< sampleHere' n

  -- | Moves to a location in a landscape that was previously sampled.
  moveTo :: forall a. (Eq a, Perturb a) => a -> Landscape a -> Maybe (Landscape a)
  moveTo a v = Landscape <$> moveIt a v
    where moveIt a = force <<< L.head <<< L.filter (\v -> (unDriverState (head v)).value == a) <<< tail <<< unLandscape

  unDriverState :: forall a. DriverState a -> DriverStateRec a
  unDriverState (DriverState v) = v

  unLandscape :: forall a. Landscape a -> Cofree L.List (DriverState a)
  unLandscape (Landscape v) = v

  mkState :: forall a. a -> Number -> GenState -> DriverState a
  mkState val var s = DriverState { value: val, variance: var, state: s }

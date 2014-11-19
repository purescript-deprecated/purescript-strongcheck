module Test.StrongCheck.Landscape
  ( DriverState(..)
  , DriverStateRec(..)
  , Landscape(..)
  , everywhere
  , everywhere'
  , moveTo
  , nearby
  , nearby'
  , sample
  , somewhere
  , somewhere'
  , unDriverState
  , unLandscape
  ) where 

  import Data.Lazy (Lazy(), force, defer)
  import Data.Maybe 
  import Data.Tuple (fst, snd)
  import Data.Monoid (mempty)

  import Control.Comonad.Cofree
  import Control.Monad.Trampoline
  import qualified Data.List.Lazy as L
  import qualified Data.Machine.Mealy as Mealy

  import qualified Data.Array as A

  import Test.StrongCheck.Perturb (Perturb, perturb)
  import Test.StrongCheck (Arbitrary, arbitrary)
  import Test.StrongCheck.Gen (GenState(..), Gen(..), toLazyList, updateSeedState, unGenOut, applyGen)

  newtype DriverState a = DriverState (DriverStateRec a)

  type DriverStateRec a = { value :: a, variance :: Number, state :: GenState }

  newtype Landscape a = Landscape (Cofree L.List (DriverState a))

  -- | Creates a landscape whose initial points are randomly chosen across
  -- | the entire landscape.
  everywhere' :: forall a. (Arbitrary a, Perturb a) => GenState -> Number -> L.List (Landscape a)
  everywhere' s v = force (go arbitrary s)
    where go :: forall a. (Arbitrary a, Perturb a) => Gen a -> GenState -> Lazy (L.List (Landscape a))
          go g s = do o   <- defer \_ -> (unGenOut $ Data.Maybe.Unsafe.fromJust (runTrampoline (applyGen s g)))
                      let a  = fst o.value
                      let g  = snd o.value
                      let s' = o.state
                      return $ L.prepend' (nearby' a s' v) (go g s')

  -- | Creates a landscape whose initial points are randomly chosen across
  -- | the entire landscape, using the default GenState.
  everywhere :: forall a. (Arbitrary a, Perturb a) => Number -> L.List (Landscape a)
  everywhere = everywhere' mempty

  -- | Picks somewhere and forms a landscape around that location.
  somewhere' :: forall a. (Arbitrary a, Perturb a) => GenState -> Number -> Landscape a
  somewhere' s = Data.Maybe.Unsafe.fromJust <<< force <<< L.head <<< everywhere' s

  -- | Picks somewhere and forms a landscape around that location, using the
  -- | default GenState.
  somewhere :: forall a. (Arbitrary a, Perturb a) => Number -> Landscape a
  somewhere = somewhere' mempty

  -- | Creates a landscape that samples the area around a location.
  nearby' :: forall a. (Perturb a) => a -> GenState -> Number -> Landscape a
  nearby' a s v = Landscape $ mkCofree (mkState a v s) (loop a s v)
    where loop a s v = 
            do  a' <- toLazyList (perturb v a) s
                let h = mkState a' v s
                let t = loop a' (updateSeedState s) (v / 2)
                return $ mkCofree h t

  -- | Creates a landscape that samples the area around a location, using the 
  -- | default GenState.
  nearby :: forall a. (Perturb a) => a -> Number -> Landscape a
  nearby a = nearby' a mempty

  -- | Samples around the current location area, returning full state information.
  sample' :: forall a. (Perturb a) => Number -> Landscape a -> [DriverState a]
  sample' n = force <<< L.toArray <<< L.take n <<< (<$>) head <<< tail <<< unLandscape

  -- | Samples around the current location area, returning just the values.
  sample :: forall a. (Perturb a) => Number -> Landscape a -> [a]
  sample n = (<$>) (unDriverState >>> \v -> v.value) <<< sample' n

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
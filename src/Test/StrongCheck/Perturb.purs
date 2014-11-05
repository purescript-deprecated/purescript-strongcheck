module Test.StrongCheck.Perturb 
  ( Attempts(..)
  , Perturb
  , Perturber(..)
  , PerturberRec(..)
  , (</\>)
  , bounded
  , boundedInt
  , dist
  , dims
  , enumerated
  , nonPerturber
  , perturb
  , perturber
  , searchIn'
  , searchIn
  , unPerturber
  , xmap
  ) where

  import Test.StrongCheck.Gen
  import Test.StrongCheck
  
  import Data.Traversable
  import Data.Foldable
  import Data.Char
  import Data.Tuple
  import Data.Monoid
  import Data.Either
  import Data.Maybe
  import Data.Maybe.Unsafe
  import Data.Monoid.Sum
  import Data.Enum
  import qualified Data.String as S
  import qualified Data.Array as A

  import Math

  import Data.Function

  newtype Attempts = Attempts Number

  newtype Perturber a = Perturber (PerturberRec a)

  type PerturberRec a = {
    perturb :: Number -> a -> Gen a,
    dist    :: a -> a -> Number,
    dims    :: a -> Number }

  unPerturber :: forall a. Perturber a -> PerturberRec a
  unPerturber (Perturber v) = v

  -- TODO: Move to Data.Functor.Invariant
  xmap :: forall a b. (a -> b) -> (b -> a) -> Perturber a -> Perturber b
  xmap f g (Perturber p) = Perturber {
    perturb : \n b -> f <$> p.perturb n (g b),
    dist    : \b1 b2 -> p.dist (g b1) (g b2),
    dims    : \b -> p.dims (g b) }

  -- | The class for things which can be perturbed.
  -- |
  -- | Laws:  
  -- |   forall a, 0 >= n <= 1:  
  -- |   ((>=) n) <<< dist a <$> (perturb n a) must be an infinite generator of `true` values.
  class Perturb a where
    perturber :: Perturber a
 
  perturb :: forall a. (Perturb a) => Number -> a -> Gen a
  perturb = (unPerturber perturber).perturb

  dist :: forall a. (Perturb a) => a -> a -> Number
  dist = (unPerturber perturber).dist

  dims :: forall a. (Perturb a) => a -> Number
  dims = (unPerturber perturber).dims

  -- | Creates a perturber that perturbs nothing.
  nonPerturber :: forall a. Perturber a
  nonPerturber = Perturber {
    perturb : const pure,
    dist    : const $ const 0,
    dims    : const 0 }

  -- | Given one example, searches for other examples that satisfy a provided
  -- | boolean predicate.
  -- | 
  -- | The search operates out-to-in, in an attempt to find examples that are 
  -- | as far removed from the provided example as possible. The sampling size
  -- | parameter determines how many samples to take at every level of 
  -- | searching, while the attempts parameter determines how many levels.
  searchIn' :: forall a. (Perturb a) => Attempts -> Number -> (a -> Boolean) -> a -> Gen a
  searchIn' (Attempts k) n f a = search0 k 1 
    where search0 k d = ifThenElse (k <= 0) mempty
                        (do a' <- find f <$> (takeGen 1 $ chunked n (perturb d a))
                            fromMaybe mempty (pure <$> a') <> search0 (k - 1) (d / 2))
  

  -- | The same as search', but uses defaults for attempt count and sample size.
  -- | Will search a total of 10,000 examples before giving up.
  searchIn :: forall a. (Perturb a) => (a -> Boolean) -> a -> Gen a
  searchIn = searchIn' (Attempts 1000) 10

  infixr 6 </\>

  -- | Combines two perturbers to produce a perturber of the product
  (</\>) :: forall a b. Perturber a -> Perturber b -> Perturber (Tuple a b)
  (</\>) (Perturber l) (Perturber r) = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where perturb' d (Tuple a b) =
            let dx = delta (l.dims a + r.dims b) d
                dx2 = dx * dx
                ld = sqrt $ dx2 * l.dims a
                rd = sqrt $ dx2 * r.dims b
            in Tuple <$> l.perturb ld a <*> r.perturb rd b

          dist' (Tuple a1 b1) (Tuple a2 b2) = toDist [l.dist a1 a2, r.dist b1 b2]

          dims' (Tuple a b) = l.dims a + r.dims b  

  -- this may make sense only for "all enums" or "all continuous"
  -- (<\/>) :: forall a b. Perturber a -> Perturber b -> Perturber (Either a b)

  -- | Creates a perturber for numbers that fall within the specified range.
  bounded :: Number -> Number -> Perturber Number
  bounded a b = 
    let l = min a b
        u = max a b

        length = u - l

        clamp n = max l (min u n)

        perturb' d v = do dx <- arbitrary
                          return <<< clamp $ dx * length * d + v

        dist' a b = abs (a - b)

        dims' = const 1

    in  Perturber { perturb : perturb', dist : dist', dims : dims' }

  -- | Creates a perturber for integers that fall within the specified range.
  boundedInt :: Number -> Number -> Perturber Number
  boundedInt a b = 
    let l = floor $ min a b
        u = ceil $ max a b

        length = u - l

        clamp n = max l (min u n)

        perturb' d v = do dx <- arbitrary
                          return <<< clamp <<< round $ dx * length * d + v

        dist' a b = abs (a - b)

        dims' = const 1

    in  Perturber { perturb : perturb', dist : dist', dims : dims' }

  enumerated :: forall a. (Eq a) => a -> [a] -> Perturber a
  enumerated x xs = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where len = 1 + A.length xs
          cutoff = 1 / (2 * len)

          perturb' n a = if n < cutoff then pure a else elements x xs
          dist' a1 a2 = if a2 == a2 then 0 else cutoff
          dims' a = if len > 0 then 1 else 0

  instance perturbArbEnum :: (Enum a) => Perturb (ArbEnum a) where
    perturber = Perturber { perturb : perturb', dist : dist', dims : dims' } 
      where perturb' n e = cardPerturb1 (cardPerturb1F e n)

            dist' a b = cardDist1 f a b where
              f (Cardinality sz) a b = if runArbEnum a == runArbEnum b then 0 else 1 / (2 * sz)

            dims' e = enumDims f e where
              f (Cardinality sz) e = if sz <= 0 then 0 else 1

  instance perturbNumber :: Perturb Number where
    perturber = Perturber { perturb : perturb', dist : dist', dims : dims' } 
      where perturb' 0 n = pure n
            perturb' d n = do
              u <- uniform -- 'up to' d
              s <- runSignum <$> arbitrary
              return $ s * (Math.exp(k0 * (u * d)) - 1) + n

            dist' a b = 
              let from y = Math.log(y + 1) / k0
              in  (Math.min 1) <<< Math.abs <<< from $ Math.abs (a - b)

            dims' = const 1

  instance perturbArray :: (Perturb a) => Perturb [a] where
    perturber = Perturber { perturb : perturb', dist : dist', dims : dims' } 
      where perturb' d []  = pure $ []
            perturb' 0 a   = sequence $ perturb 0 <$> a
            perturb' d a   = let dx = delta (A.length a) d
                             in  sequence $ perturb dx <$> a
         
            dist' a b = toDist $ A.zipWith dist a b

            dims' = A.length

  instance perturbChar :: Perturb Char where
    perturber = Perturber { perturb : perturb', dist : dist', dims : dims' } 
      where perturb' n e = if n < 1 / (2 * 65536) then pure e else (arbitrary :: Gen Char)

            dist' a b = if a == b then 0 else 1 / (2 * 65536)

            dims' = const 1

  instance perturbBoolean :: Perturb Boolean where 
    perturber = Perturber { perturb : perturb', dist : dist', dims : dims' } 
      where perturb' n e = runArbEnum <$> perturb n (ArbEnum e)

            dist' a b = dist (ArbEnum a) (ArbEnum b)

            dims' = const 1

  instance perturbString :: Perturb String where
    perturber = Perturber { perturb : perturb', dist : dist', dims : dims' } 
      where perturb' d s = S.fromCharArray <$> perturb d (S.toCharArray s)

            dist' s1 s2 = dist (S.toCharArray s1) (S.toCharArray s2)

            dims' = dims <<< S.toCharArray

  -- magical constants
  maxNumber :: Number
  maxNumber = 9007199254740992

  -- math
  k0 :: Number
  k0 = Math.log(maxNumber + 1)

  square :: Number -> Number
  square = flip Math.pow 2

  toDist :: [Number] -> Number
  toDist xs = Math.sqrt (sum $ square <$> xs)

  delta :: Number -> Number -> Number
  delta n d = Math.sqrt (d * d / n)

  -- FIXME: this workaround is still required as of psc 0.5.6.3
  ifThenElse p a b = if p then a else b

  -- ScopedTypeVariables
  enumDims :: forall a. (Enum a) => (Cardinality a -> a -> Number) -> a -> Number
  enumDims f = f cardinality

  -- ScopedTypeVariables
  cardPerturb1 :: forall f a. (Enum a) => (Cardinality a -> f a) -> f a
  cardPerturb1 f = f cardinality

  -- ScopedTypeVariables
  cardDist1 :: forall a. (Enum a) => (Cardinality a -> a -> a -> Number) -> a -> a -> Number
  cardDist1 f = f cardinality

  -- workaround to avoid:
  -- Attempted to unify a constrained type (Test.StrongCheck.Arbitrary u15286) => 
  -- Test.StrongCheck.Gen.Gen<u15286> with another type.
  cardPerturb1F :: forall a. (Enum a) => a -> Number -> Cardinality a -> Gen a
  cardPerturb1F a n (Cardinality sz) = if n < 1 / (2 * sz) then pure a else (runArbEnum <$> arbitrary)
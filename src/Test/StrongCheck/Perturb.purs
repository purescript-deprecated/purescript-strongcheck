module Test.StrongCheck.Perturb 
  ( Attempts(..)
  , Perturb
  , Perturber(..)
  , bounded
  , boundedInt
  , perturb
  , perturber2
  , perturber3
  , perturber4
  , perturber5
  , perturber6
  , perturber7
  , perturber8
  , perturber9
  , perturber10
  , dist
  , dims
  , searchIn'
  , searchIn
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

  newtype Perturber a = Perturber {
    perturb :: Number -> a -> Gen a,
    dist    :: a -> a -> Number,
    dims    :: a -> Number }

  -- | The class for things which can be perturbed.
  -- |
  -- | Laws:  
  -- |   forall a, 0 >= n <= 1:  
  -- |   ((>=) n) <<< dist a <$> (perturb n a) must be an infinite generator of `true` values.
  class Perturb a where
    perturb :: Number -> a -> Gen a

    dist :: a -> a -> Number

    dims :: a -> Number

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

  perturber2 :: forall a b. (Perturb a, Perturb b) => Perturber (Tuple a b)
  perturber2 = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where perturb' d (Tuple a b) = let dx = delta 2 d in Tuple <$> perturb dx a <*> perturb dx b
          dist' (Tuple a1 b1) (Tuple a2 b2) = toDist [dist a1 a2, dist b1 b2]
          dims' (Tuple a b) = (dims a) * (dims b)

  perturber3 :: forall a b c. (Perturb a, Perturb b, Perturb c) => Perturber (Tuple a (Tuple b c))
  perturber3 = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where perturb' d (Tuple a (Tuple b c)) = 
            let dx = delta 3 d 
            in  Tuple <$> perturb dx a <*> (Tuple <$> perturb dx b <*> perturb dx c)

          dist' (Tuple a1 (Tuple b1 c1)) (Tuple a2 (Tuple b2 c2)) = toDist [dist a1 a2, dist b1 b2, dist c1 c2]

          dims' (Tuple a (Tuple b c)) = (dims a) * (dims b) * (dims c)

  perturber4 :: forall a b c d. (Perturb a, Perturb b, Perturb c, Perturb d) => Perturber (Tuple a (Tuple b (Tuple c d)))
  perturber4 = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where perturb' d0 (Tuple a (Tuple b (Tuple c d))) = 
            let dx = delta 4 d0
            in  Tuple <$> perturb dx a <*> (Tuple <$> perturb dx b <*> (Tuple <$> perturb dx c <*> perturb dx d))

          dist' (Tuple a1 (Tuple b1 (Tuple c1 d1))) 
                (Tuple a2 (Tuple b2 (Tuple c2 d2))) = toDist [dist a1 a2, dist b1 b2, dist c1 c2, dist d1 d2]

          dims' (Tuple a (Tuple b (Tuple c d))) = (dims a) * (dims b) * (dims c) * (dims d)

  perturber5 :: forall a b c d e. (Perturb a, Perturb b, Perturb c, Perturb d, Perturb e) => Perturber (Tuple a (Tuple b (Tuple c (Tuple d e))))
  perturber5 = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where perturb' d0 (Tuple a (Tuple b (Tuple c (Tuple d e)))) = 
            let dx = delta 5 d0
            in  Tuple <$> perturb dx a <*> 
                (Tuple <$> perturb dx b <*> 
                (Tuple <$> perturb dx c <*> 
                (Tuple <$> perturb dx d <*> perturb dx e)))

          dist' (Tuple a1 (Tuple b1 (Tuple c1 (Tuple d1 e1)))) 
                (Tuple a2 (Tuple b2 (Tuple c2 (Tuple d2 e2)))) = 
            toDist [dist a1 a2, dist b1 b2, dist c1 c2, dist d1 d2, dist e1 e2]

          dims' (Tuple a (Tuple b (Tuple c (Tuple d e)))) = 
            (dims a) * (dims b) * (dims c) * (dims d) * (dims e)

  perturber6 :: forall a b c d e f. (Perturb a, Perturb b, Perturb c, Perturb d, Perturb e, Perturb f) => Perturber (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e f)))))
  perturber6 = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where perturb' d0 (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e f))))) = 
            let dx = delta 6 d0
            in  Tuple <$> perturb dx a <*> 
                (Tuple <$> perturb dx b <*> 
                (Tuple <$> perturb dx c <*> 
                (Tuple <$> perturb dx d <*> 
                (Tuple <$> perturb dx e <*> perturb dx f))))

          dist' (Tuple a1 (Tuple b1 (Tuple c1 (Tuple d1 (Tuple e1 f1))))) 
                (Tuple a2 (Tuple b2 (Tuple c2 (Tuple d2 (Tuple e2 f2))))) = 
            toDist [dist a1 a2, dist b1 b2, dist c1 c2, dist d1 d2, dist e1 e2, dist f1 f2]

          dims' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e f))))) = 
            (dims a) * (dims b) * (dims c) * (dims d) * (dims e) * (dims f)

  perturber7 :: forall a b c d e f g. (Perturb a, Perturb b, Perturb c, Perturb d, Perturb e, Perturb f, Perturb g) => Perturber (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f g))))))
  perturber7 = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where perturb' d0 (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f g)))))) = 
            let dx = delta 7 d0
            in  Tuple <$> perturb dx a <*> 
                (Tuple <$> perturb dx b <*> 
                (Tuple <$> perturb dx c <*> 
                (Tuple <$> perturb dx d <*> 
                (Tuple <$> perturb dx e <*> 
                (Tuple <$> perturb dx f <*> perturb dx g)))))

          dist' (Tuple a1 (Tuple b1 (Tuple c1 (Tuple d1 (Tuple e1 (Tuple f1 g1)))))) 
                (Tuple a2 (Tuple b2 (Tuple c2 (Tuple d2 (Tuple e2 (Tuple f2 g2)))))) = 
            toDist [dist a1 a2, dist b1 b2, dist c1 c2, dist d1 d2, dist e1 e2, dist f1 f2, dist g1 g2]

          dims' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f g)))))) = 
            (dims a) * (dims b) * (dims c) * (dims d) * (dims e) * (dims f) * (dims g)

  perturber8 :: forall a b c d e f g h. (Perturb a, Perturb b, Perturb c, Perturb d, Perturb e, Perturb f, Perturb g, Perturb h) => Perturber (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g h)))))))
  perturber8 = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where perturb' d0 (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g h))))))) = 
            let dx = delta 8 d0
            in  Tuple <$> perturb dx a <*> 
                (Tuple <$> perturb dx b <*> 
                (Tuple <$> perturb dx c <*> 
                (Tuple <$> perturb dx d <*> 
                (Tuple <$> perturb dx e <*> 
                (Tuple <$> perturb dx f <*> 
                (Tuple <$> perturb dx g <*> perturb dx h))))))

          dist' (Tuple a1 (Tuple b1 (Tuple c1 (Tuple d1 (Tuple e1 (Tuple f1 (Tuple g1 h1))))))) 
                (Tuple a2 (Tuple b2 (Tuple c2 (Tuple d2 (Tuple e2 (Tuple f2 (Tuple g2 h2))))))) = 
            toDist [dist a1 a2, dist b1 b2, dist c1 c2, dist d1 d2, dist e1 e2, dist f1 f2, dist g1 g2, dist h1 h2]

          dims' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g h))))))) = 
            (dims a) * (dims b) * (dims c) * (dims d) * (dims e) * (dims f) * (dims g) * (dims h)

  perturber9 :: forall a b c d e f g h i. (Perturb a, Perturb b, Perturb c, Perturb d, Perturb e, Perturb f, Perturb g, Perturb h, Perturb i) => Perturber (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h i))))))))
  perturber9 = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where perturb' d0 (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h i)))))))) = 
            let dx = delta 9 d0
            in  Tuple <$> perturb dx a <*> 
                (Tuple <$> perturb dx b <*> 
                (Tuple <$> perturb dx c <*> 
                (Tuple <$> perturb dx d <*> 
                (Tuple <$> perturb dx e <*> 
                (Tuple <$> perturb dx f <*> 
                (Tuple <$> perturb dx g <*> 
                (Tuple <$> perturb dx h <*> perturb dx i)))))))

          dist' (Tuple a1 (Tuple b1 (Tuple c1 (Tuple d1 (Tuple e1 (Tuple f1 (Tuple g1 (Tuple h1 i1)))))))) 
                (Tuple a2 (Tuple b2 (Tuple c2 (Tuple d2 (Tuple e2 (Tuple f2 (Tuple g2 (Tuple h2 i2)))))))) = 
            toDist [dist a1 a2, dist b1 b2, dist c1 c2, dist d1 d2, dist e1 e2, 
                    dist f1 f2, dist g1 g2, dist h1 h2, dist i1 i2]

          dims' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h i)))))))) = 
            (dims a) * (dims b) * (dims c) * (dims d) * (dims e) * (dims f) * (dims g) * (dims h) * (dims i)

  perturber10 :: forall a b c d e f g h i j. (Perturb a, Perturb b, Perturb c, Perturb d, Perturb e, Perturb f, Perturb g, Perturb h, Perturb i, Perturb j) => Perturber (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h (Tuple i j)))))))))
  perturber10 = Perturber { perturb : perturb', dist : dist', dims : dims' } 
    where perturb' d0 (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h (Tuple i j))))))))) = 
            let dx = delta 10 d0
            in  Tuple <$> perturb dx a <*> 
                (Tuple <$> perturb dx b <*> 
                (Tuple <$> perturb dx c <*> 
                (Tuple <$> perturb dx d <*> 
                (Tuple <$> perturb dx e <*> 
                (Tuple <$> perturb dx f <*> 
                (Tuple <$> perturb dx g <*> 
                (Tuple <$> perturb dx h <*> 
                (Tuple <$> perturb dx i <*> perturb dx j))))))))

          dist' (Tuple a1 (Tuple b1 (Tuple c1 (Tuple d1 (Tuple e1 (Tuple f1 (Tuple g1 (Tuple h1 (Tuple i1 j1))))))))) 
                (Tuple a2 (Tuple b2 (Tuple c2 (Tuple d2 (Tuple e2 (Tuple f2 (Tuple g2 (Tuple h2 (Tuple i2 j2))))))))) = 
            toDist [dist a1 a2, dist b1 b2, dist c1 c2, dist d1 d2, dist e1 e2, 
                    dist f1 f2, dist g1 g2, dist h1 h2, dist i1 i2, dist j1 j2]

          dims' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h (Tuple i j))))))))) = 
            (dims a) * (dims b) * (dims c) * (dims d) * (dims e) * (dims f) * (dims g) * (dims h) * (dims i) * (dims j)

  instance perturbArbEnum :: (Enum a) => Perturb (ArbEnum a) where
    perturb n e = cardPerturb1 (cardPerturb1F e n)

    dist a b = cardDist1 f a b where
      f (Cardinality sz) a b = if runArbEnum a == runArbEnum b then 0 else 1 / (2 * sz)

    dims e = enumDims f e where
      f (Cardinality sz) e = if sz <= 0 then 0 else 1

  instance perturbNumber :: Perturb Number where
    perturb 0 n = pure n
    perturb d n = do
      u <- uniform -- 'up to' d
      s <- runSignum <$> arbitrary
      return $ s * (Math.exp(k0 * (u * d)) - 1) + n

    dist a b = 
      let from y = Math.log(y + 1) / k0
      in  (Math.min 1) <<< Math.abs <<< from $ Math.abs (a - b)

    dims = const 1

  instance perturbArray :: (Perturb a) => Perturb [a] where
    perturb d []  = pure $ []
    perturb 0 a   = sequence $ perturb 0 <$> a
    perturb d a   = let dx = delta (A.length a) d
                    in  sequence $ perturb dx <$> a
 
    dist a b = toDist $ A.zipWith dist a b

    dims = A.length

  instance perturbTuple :: (Perturb a, Perturb b) => Perturb (Tuple a b) where
    perturb d (Tuple a b) = let dx = delta 2 d
                            in  Tuple <$> (perturb dx a) <*> (perturb dx b)

    dist (Tuple a1 b1) (Tuple a2 b2) = toDist [dist a1 a2, dist b1 b2]

    dims (Tuple a b) = dims a + dims b

  instance perturbChar :: Perturb Char where
    perturb n e = if n < 1 / (2 * 65536) then pure e else arbitrary

    dist a b = if a == b then 0 else 1 / (2 * 65536)

    dims = const 1

  instance perturbBoolean :: Perturb Boolean where 
    perturb n e = runArbEnum <$> perturb n (ArbEnum e)

    dist a b = dist (ArbEnum a) (ArbEnum b)

    dims = const 1

  instance perturbString :: Perturb String where
    perturb d s = S.fromCharArray <$> perturb d (S.toCharArray s)

    dist s1 s2 = dist (S.toCharArray s1) (S.toCharArray s2)

    dims = dims <<< S.toCharArray

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
module Test.StrongCheck.LCG
  ( Seed()
  , lcgM
  , lcgC
  , lcgN
  , lcgNext
  , randomSeed
  ) where

import Data.Int (Int(), fromNumber, toNumber)
import Data.Int.Bits (shl)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (Random(), random)

type Seed = Int

lcgM :: Seed
lcgM = fromNumber lcgM'

lcgM' :: Number
lcgM' = 1103515245

lcgC :: Seed
lcgC = fromNumber 12345

lcgN :: Seed
lcgN = one `shl` fromNumber 30

lcgNext :: Seed -> Seed
lcgNext n = (lcgM * n + lcgC) `mod` lcgN

randomSeed :: forall e. Eff (random :: Random | e) Seed
randomSeed = fromNumber <<< (lcgM' *) <$> random

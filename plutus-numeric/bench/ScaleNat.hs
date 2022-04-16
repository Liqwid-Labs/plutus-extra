{-# LANGUAGE QuasiQuotes #-}

module ScaleNat (benchScaleNat) where

import Data.Kind (Type)
import PlutusTx.Natural (Natural, nat, natToInteger)
import PlutusTx.Numeric.Extra (scaleNat)
import PlutusTx.Prelude qualified as P
import Test.Tasty
import Test.Tasty.Bench

naiveScaleNat ::
  forall (a :: Type).
  (P.AdditiveMonoid a) =>
  Natural ->
  a ->
  a
naiveScaleNat i a = go (natToInteger i)
  where
    go :: Integer -> a
    go x
      | x == P.zero = P.zero
      | otherwise = a P.+ go (x - 1)

benchScaleNat :: TestTree
benchScaleNat =
  testGroup
    "scaleNat"
    [ testGroup
        "naive"
        [ bench "1000" $ nf (naiveScaleNat @Integer [nat|1000|]) 1000
        , bench "10000" $ nf (naiveScaleNat @Integer [nat|10000|]) 1000
        , bench "100000" $ nf (naiveScaleNat @Integer [nat|100000|]) 1000
        ]
    , testGroup
        "expBySq"
        [ bench "1000" $ nf (scaleNat @Integer [nat|1000|]) 1000
        , bench "10000" $ nf (scaleNat @Integer [nat|10000|]) 1000
        , bench "100000" $ nf (scaleNat @Integer [nat|100000|]) 1000
        ]
    ]

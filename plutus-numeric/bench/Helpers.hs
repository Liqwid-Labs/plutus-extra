{-# LANGUAGE QuasiQuotes #-}

module Helpers (
  mkScaleNatBenches,
  ten,
  hundred,
  thousand,
  tenThousand,
) where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import PlutusTx.Natural (Natural, nat, natToInteger)
import PlutusTx.Numeric.Extra (scaleNat)
import PlutusTx.Prelude qualified as P
import Test.Tasty.Bench (Benchmark, bcompare, bench, bgroup, nf)

{-# INLINE mkScaleNatBenches #-}
mkScaleNatBenches ::
  forall (a :: Type).
  (NFData a, P.AdditiveMonoid a) =>
  String ->
  a ->
  [Natural] ->
  Benchmark
mkScaleNatBenches name target = bgroup name . foldMap go
  where
    go :: Natural -> [Benchmark]
    go n =
      let nString = show n
          ruleHead = "$2 == \"" <> name <> "\" && "
          rewrittenName = "rewritten, " <> nString
          ebsName = "EBS fallback, " <> nString
          naiveName = "naive, " <> nString
          ebsRule = ruleHead <> "$NF == \"" <> rewrittenName <> "\""
          naiveRule = ruleHead <> "$NF == \"" <> rewrittenName <> "\""
       in [ bench rewrittenName . nf (scaleNat n) $ target
          , bcompare ebsRule . bench ebsName . nf (ebsScaleNat n) $ target
          , bcompare naiveRule . bench naiveName . nf (naiveScaleNat n) $ target
          ]

ten :: Natural
ten = [nat| 10 |]

hundred :: Natural
hundred = [nat| 100 |]

thousand :: Natural
thousand = [nat| 1000 |]

tenThousand :: Natural
tenThousand = [nat| 10000 |]

-- Helpers

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

ebsScaleNat ::
  forall (a :: Type).
  (P.AdditiveMonoid a) =>
  Natural ->
  a ->
  a
ebsScaleNat n x =
  let i = natToInteger n
   in if i == P.zero
        then P.zero
        else go x i
  where
    go :: a -> Integer -> a
    go acc i
      | i == P.one = acc
      | even' i = go (double acc) . halve $ i
      | otherwise = (acc P.+) . go (double acc) . halve $ i
    even' :: Integer -> Bool
    even' i = i `P.remainder` 2 == P.zero
    double :: a -> a
    double y = y P.+ y
    halve :: Integer -> Integer
    halve = (`P.divide` 2)

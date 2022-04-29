{-# LANGUAGE QuasiQuotes #-}

module Helpers (
  mkSemiscaleBench,
  ten,
  hundred,
  thousand,
  tenThousand,
) where

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import PlutusTx.Natural (Natural, nat)
import PlutusTx.Numeric.Extra (Semimodule (..))
import Test.Tasty.Bench (Benchmark, bench, bgroup, nf)

{-# INLINE mkSemiscaleBench #-}
mkSemiscaleBench ::
  forall (a :: Type).
  (NFData a, Semimodule Natural a) =>
  String ->
  a ->
  [Natural] ->
  Benchmark
mkSemiscaleBench name target = bgroup name . foldMap go
  where
    go :: Natural -> [Benchmark]
    go n =
      let nString = show n
          rewrittenName = "rewritten, " <> nString
       in [ bench rewrittenName . nf (semiscale n) $ target
          ]

ten :: Natural
ten = [nat| 10 |]

hundred :: Natural
hundred = [nat| 100 |]

thousand :: Natural
thousand = [nat| 1000 |]

tenThousand :: Natural
tenThousand = [nat| 10000 |]

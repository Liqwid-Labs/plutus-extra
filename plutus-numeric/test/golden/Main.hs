module Main (main) where

import Data.Kind (Type)
import PlutusTx.NatRatio (NatRatio)
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (restrictMay)
import PlutusTx.Ratio qualified as R
import System.Random.Stateful (StatefulGen, uniformRM)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Generator (Generator (Generator))
import Test.Tasty.Plutus.Golden (goldenJSON, goldenData)

main :: IO ()
main =
  defaultMain . testGroup "Golden tests" $
    [ goldenJSON genNatural
    , goldenJSON genNatRatio
    , goldenData genNatural
    , goldenData genNatRatio
    ]

-- Helpers

highLimit :: Integer
highLimit = 1_000_000_000

genNatural :: Generator Natural
genNatural = Generator go
  where
    go ::
      forall (g :: Type) (m :: Type -> Type).
      (StatefulGen g m) =>
      g ->
      m Natural
    go rng = do
      i <- uniformRM (0, highLimit) rng
      case restrictMay i of
        Nothing -> go rng -- won't happen, but makes compiler happy
        Just n -> pure n

genNatRatio :: Generator NatRatio
genNatRatio = Generator go
  where
    go ::
      forall (g :: Type) (m :: Type -> Type).
      (StatefulGen g m) =>
      g ->
      m NatRatio
    go rng = do
      num <- uniformRM (0, highLimit) rng
      den <- uniformRM (1, highLimit) rng
      case restrictMay $ num R.% den of
        Nothing -> go rng -- won't happen, but makes compiler happy
        Just nr -> pure nr

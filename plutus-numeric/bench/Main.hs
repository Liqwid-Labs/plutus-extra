{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Helpers (
  hundred,
  mkScaleNatBenches,
  ten,
  tenThousand,
  thousand,
 )
import PlutusTx.NatRatio (NatRatio)
import PlutusTx.NatRatio qualified as NatRatio
import PlutusTx.Natural (Natural, nat)
import PlutusTx.Ratio (Rational)
import PlutusTx.Rational.QQ qualified as Rational
import Test.Tasty.Bench (defaultMain)
import Prelude hiding (Rational)

main :: IO ()
main =
  defaultMain
    [ mkScaleNatBenches "Natural" nTarget [ten, hundred, thousand, tenThousand]
    , mkScaleNatBenches "Integer" iTarget [ten, hundred, thousand, tenThousand]
    , mkScaleNatBenches "NatRatio" nrTarget [ten, hundred, thousand, tenThousand]
    , mkScaleNatBenches "Rational" rTarget [ten, hundred, thousand, tenThousand]
    ]
  where
    nTarget :: Natural
    nTarget = [nat| 1234 |]
    iTarget :: Integer
    iTarget = 1234
    nrTarget :: NatRatio
    nrTarget = [NatRatio.frac| (13,29) |]
    rTarget :: Rational
    rTarget = [Rational.frac| (13,29) |]

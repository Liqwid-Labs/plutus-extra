{-# LANGUAGE Trustworthy #-}

{- |
 Module: PlutusTx.NatRatio
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 An on-chain numeric type representing a ratio of non-negative numbers.
-}
module PlutusTx.NatRatio (
  -- * Type
  Internal.NatRatio,

  -- * Functions

  -- ** Construction
  Internal.fromNatural,
  Internal.natRatio,
  QQ.dec,
  QQ.frac,

  -- ** Access
  Internal.numerator,
  Internal.denominator,
  Internal.truncate,
  Internal.ceiling,
  Internal.round,
  Internal.properFraction,
  Internal.recip,
  Internal.toRational,

  -- ** NatRatio
  Internal.NatRatioSchema (NatRatioSchema)
) where

import PlutusTx.NatRatio.Internal qualified as Internal
import PlutusTx.NatRatio.QQ qualified as QQ

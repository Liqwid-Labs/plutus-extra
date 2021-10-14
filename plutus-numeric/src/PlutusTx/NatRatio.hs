-- | An on-chain numeric type representing a ratio of non-negative numbers.
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
) where

import PlutusTx.NatRatio.Internal qualified as Internal
import PlutusTx.NatRatio.QQ qualified as QQ

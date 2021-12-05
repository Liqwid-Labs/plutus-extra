{-# LANGUAGE NoImplicitPrelude #-}

module PlutusTx.Rational (
  Internal.Rational,
  (Internal.%),
  Internal.negate,
  Internal.fromInteger,
  Internal.numerator,
  Internal.denominator,
  Internal.properFraction,
  Internal.recip,
  Internal.abs,
  Internal.truncate,
  Internal.round,
  Internal.half,
  Internal.fromGHC,
  Internal.toGHC,
) where

import PlutusTx.Rational.Internal qualified as Internal

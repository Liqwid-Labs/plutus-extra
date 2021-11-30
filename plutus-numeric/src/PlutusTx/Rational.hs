{-# LANGUAGE NoImplicitPrelude #-}

module PlutusTx.Rational (
  Internal.Rational,
  Internal.mkRational,
  Internal.negate,
  Internal.fromInteger,
  Internal.numerator,
  Internal.denominator,
  Internal.properFraction,
  Internal.recip,
  Internal.abs,
  Internal.truncate,
  Internal.round,
) where

import PlutusTx.Rational.Internal qualified as Internal
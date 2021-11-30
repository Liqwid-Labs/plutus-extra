{-# LANGUAGE NoImplicitPrelude #-}

module PlutusTx.Rational (
  Internal.Rational,
  Internal.negate,
  Internal.fromInteger,
  Internal.numerator,
  Internal.denominator,
  Internal.properFraction,
  Internal.recip,
  Internal.abs,
) where

import PlutusTx.Rational.Internal qualified as Internal

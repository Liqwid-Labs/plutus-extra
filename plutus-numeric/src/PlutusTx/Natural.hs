{-# LANGUAGE Trustworthy #-}

{- |
 Module: PlutusTx.Natural
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 An on-chain numeric type representing non-negative integers.
-}
module PlutusTx.Natural (
  -- * Types,
  Internal.Natural,
  Internal.Parity (..),

  -- * Quasi-quoter
  QQ.nat,

  -- * Functions
  Internal.parity,
  natToInteger,
) where

import PlutusTx.Natural.Internal as Internal
import PlutusTx.Natural.QQ as QQ
import PlutusTx.Numeric.Extra (addExtend)
import PlutusTx.Prelude qualified as PTx

{- | The same as 'addExtend', but specialized for the
 'Internal.Natural' to 'PTx.Integer' case.

 @since 2.1
-}
natToInteger :: Internal.Natural -> PTx.Integer
natToInteger = addExtend

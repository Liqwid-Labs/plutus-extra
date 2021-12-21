{- |
 Module: PlutusTx.Positive
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Sergey Kurgak <sergey@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 An on-chain numeric type representing positive integers.
-}
module PlutusTx.Positive (
  -- * Types,
  Internal.Positive,

  -- * Functions
  getPositive,

  -- * Quasi-quoter
  QQ.positive,
) where

import PlutusTx.Positive.Internal as Internal
import PlutusTx.Positive.QQ as QQ

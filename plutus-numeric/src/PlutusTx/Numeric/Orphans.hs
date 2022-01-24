{-# OPTIONS_GHC -Wno-orphans #-}

{- | Module: PlutusTx.Numeric.Orphans
 Copyright: (C) MLabs 2022
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A collection of orphan instances for numeric types we don't control.

 This module does not have an export list; to make use of its instances, do
 the following:

 > import PlutusTx.Numeric.Orphans ()
-}
module PlutusTx.Numeric.Orphans () where

import Data.OpenApi (ToSchema)
import PlutusTx.Ratio qualified as Ratio
import PlutusTx.SchemaUtils (RatioFields ((:%:)), RatioSchema)

-- | @since 5.0
deriving via
  (RatioSchema ("numerator" ':%: "denominator"))
  instance
    ToSchema Ratio.Rational

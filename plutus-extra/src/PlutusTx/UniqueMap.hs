{- | An association data structure, suitable for on-chain use, which preserves
 the uniqueness of its keys.

 This module is designed for importing qualified. The interface is based on
 'PlutusTx.AssocMap', but with less boolean blindness.

 = Note

 All time complexities assume \(\Theta(1)\) order comparisons for keys.
-}
module PlutusTx.UniqueMap (
  -- * Types
  Internal.UniqueMap,
  Internal.Inclusion (..),

  -- * Construction
  Internal.empty,
  Internal.singleton,
  Internal.fromList,

  -- * Queries
  Internal.size,
  Internal.lookup,
  Internal.findMin,
  Internal.findMax,
  Internal.inclusion,

  -- * Updates
  Internal.insert,
  Internal.delete,
  Internal.alter,
  Internal.mapWithKey,
  Internal.mapMaybe,
  Internal.mapMaybeWithKey,

  -- * Combination
  Internal.union,
  Internal.unionWith,

  -- * Conversion
  Internal.toList,
  Internal.keys,
  Internal.elems,
) where

import PlutusTx.UniqueMap.Internal qualified as Internal

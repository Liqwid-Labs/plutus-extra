{-# LANGUAGE Trustworthy #-}

{- |
 Module: PlutusTx.AssocMap.Natural
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Xiaoyan Ren <xiaoyan@mlabs.city>
 Portability: GHC only
 Stability: Experimental
 AssocMap functions that works with the 'Natural' type.
-}
module PlutusTx.AssocMap.Natural (
  take,
  drop,
  splitAt,
) where

import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.List.Natural qualified as List
import PlutusTx.Natural (Natural)
import PlutusTx.Prelude hiding (take)
import Prelude qualified ()

{- | @'take' n mp@ returns the prefix of @mp@ of length @n@, or @mp@ itself if
@n@ exceeds the size of @mp@.

@since 3.0
-}
{-# INLINEABLE take #-}
take :: Natural -> Map k v -> Map k v
take n = AssocMap.fromList . List.take n . AssocMap.toList

{- | @'drop' n mp@ returns the suffix of @mp@ after the first @n@ elements, or
an empty map if @n@ exceeds the size of @mp@.

@since 3.0
-}
{-# INLINEABLE drop #-}
drop :: Natural -> Map k v -> Map k v
drop n = AssocMap.fromList . List.drop n . AssocMap.toList

{- | @'splitAt' n mp@ returns a tuple, where the first element is the prefix of
@mp@ of length @n@, and the second element is the remainder of the map.

@since 3.0
-}
{-# INLINEABLE splitAt #-}
splitAt :: Natural -> Map k v -> (Map k v, Map k v)
splitAt n mp =
  let (xs, ys) = List.splitAt n . AssocMap.toList $ mp
   in (AssocMap.fromList xs, AssocMap.fromList ys)

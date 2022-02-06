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

{-# INLINEABLE take #-}
take :: Natural -> Map k v -> Map k v
take n = AssocMap.fromList . List.take n . AssocMap.toList

{-# INLINEABLE drop #-}
drop :: Natural -> Map k v -> Map k v
drop n = AssocMap.fromList . List.drop n . AssocMap.toList

{-# INLINEABLE splitAt #-}
splitAt :: Natural -> Map k v -> (Map k v, Map k v)
splitAt n mp =
  let (xs, ys) = List.splitAt n . AssocMap.toList $ mp
   in (AssocMap.fromList xs, AssocMap.fromList ys)

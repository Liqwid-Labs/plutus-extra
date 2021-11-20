{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Trustworthy #-}

{- |
 Module: PlutusTx.List.Natural
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Xiaoyan Ren <xiaoyan@mlabs.city>
 Portability: GHC only
 Stability: Experimental
 List functions that works with the 'Natural' type.
-}
module PlutusTx.List.Natural (
  length,
  replicate,
  take,
  drop,
  splitAt,
) where

import Data.Monoid (Sum (Sum, getSum))
import PlutusTx.Natural (Natural, nat)
import PlutusTx.Prelude hiding (length, take)
import Prelude qualified ()

{- | Returns the size/length of a finite structure as a 'Natural'.

@since 1.0
-}
{-# INLINEABLE length #-}
length :: Foldable f => f a -> Natural
length = getSum . foldMap (Sum . const [nat| 1 |])

{- | @'replicate' n x@ is a list of length @n@ with @x@ the value of every
element.

@since 1.0
-}
{-# INLINEABLE replicate #-}
replicate :: Natural -> a -> [a]
replicate [nat| 0 |] _ = []
replicate n x = x : replicate (pred n) x

{- | @'take' n xs@ returns the prefix of @xs@ of length @n@, or @xs@ itself if
@n '>=' 'length' xs@.

@since 1.0
-}
{-# INLINEABLE take #-}
take :: Natural -> [a] -> [a]
take _ [] = []
take [nat| 0 |] _ = []
take n (x : xs) = x : take (pred n) xs

{- | @'drop' n xs@ returns the suffix of @xs@ after the first @n@ elements, or
@[]@ if @n '>=' 'length' xs@.

@since 1.0
-}
{-# INLINEABLE drop #-}
drop :: Natural -> [a] -> [a]
drop _ [] = []
drop [nat| 0 |] xs = xs
drop n (_ : xs) = drop (pred n) xs

{- | @'splitAt' n xs@ returns a tuple, where the first element is the prefix of
@xs@ of length @n@, and the second element is the remainder of the list.

@since 1.0
-}
{-# INLINEABLE splitAt #-}
splitAt :: Natural -> [a] -> ([a], [a])
splitAt _ [] = ([], [])
splitAt [nat| 0 |] xs = ([], xs)
splitAt n (x : xs) =
  let (xs', xs'') = splitAt (pred n) xs in (x : xs', xs'')

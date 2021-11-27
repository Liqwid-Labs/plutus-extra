{-# LANGUAGE Trustworthy #-}

{- |
 Module: PlutusTx.List.Ord
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Xiaoyan Ren <xiaoyan@mlabs.city>
 Portability: GHC only
 Stability: Experimental
 Functions for checking the sortedness of lists.
-}
module PlutusTx.List.Ord (
  isSorted,
  isSortedOn,
  isSortedAscending,
  isSortedAscendingOn,
  isSortedBy,
  sort,
  sortOn,
  sortBy,
  ordNub,
  ordNubBy,
) where

import Data.Function (on)
import Data.Kind (Type)
import PlutusTx.Prelude
import Prelude qualified ()

{- | Checks if the 'Foldable' is sorted with respect to a total order
represented by a binary relation.

@since 1.0
-}
{-# INLINEABLE isSortedBy #-}
isSortedBy :: Foldable f => (a -> a -> Bool) -> f a -> Bool
isSortedBy f = snd . foldr go (Nothing, True)
  where
    go x (Nothing, _) = (Just x, True)
    go x (Just y, b) = (Just x, b /\ f x y)

{- | Checks if the 'Foldable' is sorted in ascending order with respect to the
result of a key function.

@since 1.0
-}
{-# INLINEABLE isSortedOn #-}
isSortedOn :: (Foldable f, Ord b) => (a -> b) -> f a -> Bool
isSortedOn f = isSortedBy (\x y -> f x <= f y)

{-# INLINEABLE isSorted #-}

{- | Checks if the 'Foldable' is sorted in ascending order with respect to its
elements' 'Ord' instance.

@since 1.0
-}
isSorted :: (Foldable f, Ord a) => f a -> Bool
isSorted = isSortedBy (<=)

{- | Checks if the 'Foldable' is sorted strictly in ascending order with respect
to the result of a key function.. This means any pair of adjacent elements
cannot be equal.

@since 1.0
-}
{-# INLINEABLE isSortedAscendingOn #-}
isSortedAscendingOn :: (Foldable f, Ord b) => (a -> b) -> f a -> Bool
isSortedAscendingOn f = isSortedBy (\x y -> f x < f y)

{- | Checks if the 'Foldable' is sorted strictly in ascending order with respect
to its elements' 'Ord' instance. This means any pair of adjacent elements cannot
be equal.

@since 1.0
-}
{-# INLINEABLE isSortedAscending #-}
isSortedAscending :: (Foldable f, Ord a) => f a -> Bool
isSortedAscending = isSortedBy (<)

{- | The inlineable version of @sort@ in @base@.

 @since 1.0
-}
{-# INLINEABLE sort #-}
sort :: forall (a :: Type). (Ord a) => [a] -> [a]
sort = sortBy compare

{- | The inlineable version of @sortOn@ in @base@.

 @since 1.0
-}
{-# INLINEABLE sortOn #-}
sortOn :: forall (a :: Type) (b :: Type). (Ord b) => (a -> b) -> [a] -> [a]
sortOn = sortBy . (compare `on`)

{- | The inlineable version of @sortBy@ in @base@.

 @since 1.0
-}
{-# INLINEABLE sortBy #-}
sortBy :: forall (a :: Type). (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    sequences (a : b : xs)
      | a `cmp` b == GT = descending b [a] xs
      | otherwise = ascending b (a :) xs
    sequences xs = [xs]

    descending a as (b : bs)
      | a `cmp` b == GT = descending b (a : as) bs
    descending a as bs = (a : as) : sequences bs

    ascending a as (b : bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a : ys)) bs
    ascending a as bs =
      let !x = as [a]
       in x : sequences bs

    mergeAll [x] = x
    mergeAll xs = mergeAll (mergePairs xs)

    mergePairs (a : b : xs) =
      let !x = merge a b
       in x : mergePairs xs
    mergePairs xs = xs

    merge as@(a : as') bs@(b : bs')
      | a `cmp` b == GT = b : merge as bs'
      | otherwise = a : merge as' bs
    merge [] bs = bs
    merge as [] = as

{- | \( \mathcal{O}(n \log n) \). This function removes duplicate elements in a
 list and sorts the list in ascending order.

 @since 1.0
-}
{-# INLINEABLE ordNub #-}
ordNub :: forall (a :: Type). Ord a => [a] -> [a]
ordNub = ordNubBy compare

{- | Non-overloaded version of 'ordNub' that accepts any total order.

 @since 1.0
-}
{-# INLINEABLE ordNubBy #-}
ordNubBy :: forall (a :: Type). (a -> a -> Ordering) -> [a] -> [a]
ordNubBy f = go . sortBy f
  where
    go (x : y : xs)
      | f x y == EQ = go (y : xs)
      | otherwise = x : go (y : xs)
    go xs = xs

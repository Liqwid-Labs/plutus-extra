{-# LANGUAGE Trustworthy #-}

{- |
 Module: PlutusTx.List.Ord
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Xiaoyan Ren <xiaoyan@mlabs.city>
 Portability: GHC only
 Stability: Experimental
 Functions related to sorting lists.
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

import Data.Kind (Type)
import PlutusTx.Prelude
import Prelude qualified ()

{- | Checks if the 'Foldable' is sorted with respect to a total order
represented by a binary relation.

@since 1.0
-}
{-# INLINEABLE isSortedBy #-}
isSortedBy ::
  forall (f :: Type -> Type) (a :: Type).
  Foldable f =>
  (a -> a -> Bool) ->
  f a ->
  Bool
isSortedBy f = snd . foldr go (Nothing, True)
  where
    go x (Nothing, _) = (Just x, True)
    go x (Just y, b) = (Just x, b /\ f x y)

{- | Checks if the 'Foldable' is sorted in ascending order with respect to the
result of a key function.

@since 1.0
-}
{-# INLINEABLE isSortedOn #-}
isSortedOn ::
  forall (f :: Type -> Type) (a :: Type) (b :: Type).
  (Foldable f, Ord b) =>
  (a -> b) ->
  f a ->
  Bool
isSortedOn f = isSortedBy (\x y -> f x <= f y)

{- | Checks if the 'Foldable' is sorted in ascending order with respect to its
elements' 'Ord' instance.

@since 1.0
-}
{-# INLINEABLE isSorted #-}
isSorted ::
  forall (f :: Type -> Type) (a :: Type).
  (Foldable f, Ord a) =>
  f a ->
  Bool
isSorted = isSortedBy (<=)

{- | Checks if the 'Foldable' is sorted strictly in ascending order with respect
to the result of a key function.. This means any pair of adjacent elements
cannot be equal.

@since 1.0
-}
{-# INLINEABLE isSortedAscendingOn #-}
isSortedAscendingOn ::
  forall (f :: Type -> Type) (a :: Type) (b :: Type).
  (Foldable f, Ord b) =>
  (a -> b) ->
  f a ->
  Bool
isSortedAscendingOn f = isSortedBy (\x y -> f x < f y)

{- | Checks if the 'Foldable' is sorted strictly in ascending order with respect
to its elements' 'Ord' instance. This means any pair of adjacent elements cannot
be equal.

@since 1.0
-}
{-# INLINEABLE isSortedAscending #-}
isSortedAscending ::
  forall (f :: Type -> Type) (a :: Type).
  (Foldable f, Ord a) =>
  f a ->
  Bool
isSortedAscending = isSortedBy (<)

{- | The sort function implements a stable sorting algorithm. It is a special
case of 'sortBy', which allows the programmer to supply their own comparison
function.

Elements are arranged from lowest to highest, keeping duplicates in the order
they appeared in the input.

>>> sort [1,6,4,3,2,5]
[1,2,3,4,5,6]

 @since 1.0
-}
{-# INLINEABLE sort #-}
sort :: forall (a :: Type). (Ord a) => [a] -> [a]
sort = sortBy compare

{- | Sort a list by comparing the results of a key function applied to each
element.

Elements are arranged from lowest to highest, keeping duplicates in the order
they appeared in the input.

>>> sortOn fst [(2, "world"), (4, "!"), (1, "Hello")]
[(1,"Hello"),(2,"world"),(4,"!")]

 @since 1.0
-}
{-# INLINEABLE sortOn #-}
sortOn :: forall (a :: Type) (b :: Type). (Ord b) => (a -> b) -> [a] -> [a]
sortOn f = sortBy $ \x y -> compare (f x) (f y)

{- | The 'sortBy' function is the non-overloaded function of 'sort'. In order
to reduce code size, this is a naive implementation of mergesort, slower than
the version in @base@.

 @since 1.0
-}
{-# INLINEABLE sortBy #-}
sortBy :: forall (a :: Type). (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . map (: [])
  where
    mergeAll [] = []
    mergeAll [t] = t
    mergeAll xs = mergeAll (mergePairs xs)

    mergePairs (x : y : xs) = merge x y : mergePairs xs
    mergePairs xs = xs

    merge [] ys = ys
    merge xs [] = xs
    merge (x : xs) (y : ys)
      | x `cmp` y == GT = y : merge (x : xs) ys
      | otherwise = x : merge xs (y : ys)

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

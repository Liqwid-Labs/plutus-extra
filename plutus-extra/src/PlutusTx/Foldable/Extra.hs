{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusTx.Foldable.Extra (isSubsetOf, firstJust, maximum, minimum) where

--------------------------------------------------------------------------------

import Data.Kind (Type)
import Data.Monoid (First (First, getFirst))

--------------------------------------------------------------------------------

import PlutusTx.Foldable
import PlutusTx.Ord
import PlutusTx.Prelude

--------------------------------------------------------------------------------

{- | @ xs `isSubsetOf` ys @ indicates whether xs is a subset of ys
   O(n*m), n<=m
-}
isSubsetOf :: forall (t :: Type -> Type) (a :: Type). (Foldable t, Eq a) => t a -> t a -> Bool
isSubsetOf xs ys = all (`elem` ys) xs -- Can make more efficient later if needed

{- | Finds the first element of a list for which the given function
       returns a Just value.
   Equivalent to `mapMaybe . listToMaybe` but faster without laziness
-}
{-# INLINEABLE firstJust #-}
firstJust ::
  forall (t :: Type -> Type) (a :: Type) (b :: Type).
  Foldable t =>
  (a -> Maybe b) ->
  t a ->
  Maybe b
firstJust f = getFirst . foldMap (First . f)

{-# INLINEABLE maximum #-}
maximum :: forall (t :: Type -> Type) (a :: Type). (Foldable t, Ord a) => t a -> Maybe a
maximum xs = getMax <$> foldMap (Just . Max) xs

{-# INLINEABLE minimum #-}
minimum :: forall (t :: Type -> Type) (a :: Type). (Foldable t, Ord a) => t a -> Maybe a
minimum xs = getMin <$> foldMap (Just . Min) xs

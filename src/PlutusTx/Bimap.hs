{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas
  -fno-strictness
  -fno-specialize #-}

module PlutusTx.Bimap (
  Bimap,
  empty,
  fromList,
  null,
  toList,
  lookup,
  lookupR,
  size,
  insert,
  deleteL,
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)
import GHC.Generics (Generic)
import Prelude qualified

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)

--------------------------------------------------------------------------------

import PlutusTx qualified (makeLift)
import PlutusTx.IsData.Class
import PlutusTx.Prelude hiding (null, toList)

--------------------------------------------------------------------------------

import PlutusTx.Set (Set)
import PlutusTx.Set qualified as Set

--------------------------------------------------------------------------------

{- | A Bimap is just a 'Set', however it has no unique keys on either end.

  The pairs are unique together, and their order is not ignored.
  Specifically, if `a ~ b`, then `(a0, a1)` is distinctly different to `(a1, a0)`

  This Bimap (unlike the package `bimap`) does not actually form a bijective map.

  Why? Because there can be bimaps which have multiple arrows going to one element.
  @
    Bimap.fromList [ (a, z), (b, z), (c, z) ]
  @
-}
newtype Bimap (a :: Type) (b :: Type) = Bimap {unBimap :: Set (a, b)}
  deriving stock (Prelude.Show, Prelude.Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

instance (ToData a, ToData b) => ToData (Bimap a b) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = toBuiltinData . unBimap

instance (Ord a, Ord b, FromData a, FromData b) => FromData (Bimap a b) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = fmap Bimap . fromBuiltinData

instance (Ord a, Ord b, UnsafeFromData a, UnsafeFromData b) => UnsafeFromData (Bimap a b) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = Bimap . unsafeFromBuiltinData

-- | Lookup the right-side elements by searching for a left-side key
{-# INLINEABLE lookup #-}
lookup :: forall (a :: Type) (b :: Type). Eq a => a -> Bimap a b -> [b]
lookup a = fmap snd . filter ((== a) . fst) . Set.toList . unBimap

-- | Lookup the left-side elements by searching for a right-side key
{-# INLINEABLE lookupR #-}
lookupR :: forall (a :: Type) (b :: Type). Eq b => b -> Bimap a b -> [a]
lookupR b = fmap fst . filter ((== b) . snd) . Set.toList . unBimap

-- | How many pairs are in the Bimap
{-# INLINEABLE size #-}
size :: forall (a :: Type) (b :: Type). Bimap a b -> Integer
size = Set.size . unBimap

-- | An empty Bimap
{-# INLINEABLE empty #-}
empty :: forall (a :: Type) (b :: Type). (Ord a, Ord b) => Bimap a b
empty = Bimap Set.empty

-- | Check if the Bimap is empty
{-# INLINEABLE null #-}
null :: forall (a :: Type) (b :: Type). Bimap a b -> Bool
null = Set.null . unBimap

{-# INLINEABLE fromList #-}

-- | Create a `Bimap` from a list of pairs.
fromList :: forall (a :: Type) (b :: Type). (Ord a, Ord b) => [(a, b)] -> Bimap a b
fromList = Bimap . Set.fromList

{-# INLINEABLE toList #-}

-- | Convert the `Bimap` to a list of pairs.
toList :: forall (a :: Type) (b :: Type). Bimap a b -> [(a, b)]
toList = Set.toList . unBimap

-- | Insert a pair into the bimap
{-# INLINEABLE insert #-}
insert :: forall (a :: Type) (b :: Type). (Ord a, Ord b) => a -> b -> Bimap a b -> Bimap a b
insert a b = Bimap . Set.insert (a, b) . unBimap

{-# INLINEABLE deleteL #-}

-- | Delete all pairs that have a specific left-side element.
deleteL :: forall (a :: Type) (b :: Type). Eq a => a -> Bimap a b -> Bimap a b
deleteL a = Bimap . Set.filter ((/= a) . fst) . unBimap

PlutusTx.makeLift ''Bimap

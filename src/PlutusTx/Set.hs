{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas
  -fno-strictness
  -fno-specialize #-}

module PlutusTx.Set (
  Set,
  empty,
  singleton,
  insert,
  toList,
  fromList,
  null,
  member,
  size,
  all,
  delete,
  filter,
  map,
  union,
) where

--------------------------------------------------------------------------------

import GHC.Generics
import Prelude qualified

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON (parseJSON), ToJSON)
import Data.Kind (Type)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import PlutusTx qualified (makeLift)
import PlutusTx.IsData.Class (IsData (fromBuiltinData, toBuiltinData, unsafeFromBuiltinData))
import PlutusTx.Prelude hiding (all, filter, foldMap, map, null, toList)
import PlutusTx.Prelude qualified

--------------------------------------------------------------------------------

newtype Set (a :: Type) = Set {unSet :: [a]}
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving newtype (ToJSON)

deriving newtype instance Eq a => Eq (Set a)
deriving newtype instance Ord a => Ord (Set a)

instance Ord a => Semigroup (Set a) where
  {-# INLINEABLE (<>) #-}
  (<>) = union

instance Ord a => Monoid (Set a) where
  {-# INLINEABLE mempty #-}
  mempty = empty

instance Foldable Set where
  {-# INLINEABLE foldMap #-}
  foldMap :: forall (m :: Type) (a :: Type). Monoid m => (a -> m) -> Set a -> m
  foldMap f = mconcat . fmap f . toList

instance (Ord a, FromJSON a) => FromJSON (Set a) where
  {-# INLINEABLE parseJSON #-}
  parseJSON = Prelude.fmap fromList . parseJSON

instance (Ord a, IsData a) => IsData (Set a) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = toBuiltinData . toList
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = PlutusTx.Prelude.fmap fromList . fromBuiltinData
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = fromList . unsafeFromBuiltinData

{-# INLINEABLE empty #-}
empty :: forall (a :: Type). Ord a => Set a
empty = fromList []

{-# INLINEABLE singleton #-}
singleton :: forall (a :: Type). Ord a => a -> Set a
singleton = fromList . (: [])

{-# INLINEABLE insert #-}
insert :: forall (a :: Type). Ord a => a -> Set a -> Set a
insert n = Set . go . unSet
  where
    go :: [a] -> [a]
    go [] = [n]
    go lst@(x : xs) = case compare n x of
      LT -> n : lst
      EQ -> lst
      GT -> x : go xs

{-# INLINEABLE toList #-}
toList :: forall (a :: Type). Set a -> [a]
toList = unSet

{-# INLINEABLE fromList #-}
fromList :: forall (a :: Type). Ord a => [a] -> Set a
fromList = foldr insert (Set [])

{-# INLINEABLE null #-}
null :: forall (a :: Type). Set a -> Bool
null = PlutusTx.Prelude.null . unSet

{-# INLINEABLE member #-}
member :: forall (a :: Type). Ord a => a -> Set a -> Bool
member n = go . unSet
  where
    go :: [a] -> Bool
    go [] = False
    go (x : xs) = case compare n x of
      LT -> False
      EQ -> True
      GT -> go xs

{-# INLINEABLE size #-}
size :: forall (a :: Type). Set a -> Integer
size = length . toList

{-# INLINEABLE all #-}
all :: forall (a :: Type). (a -> Bool) -> Set a -> Bool
all predicate = PlutusTx.Prelude.all predicate . toList

{-# INLINEABLE delete #-}
delete :: forall (a :: Type). Ord a => a -> Set a -> Set a
delete n = Set . go . unSet
  where
    go :: [a] -> [a]
    go [] = []
    go lst@(x : xs) = case compare n x of
      LT -> lst
      EQ -> xs
      GT -> x : go xs

{-# INLINEABLE filter #-}
filter :: forall (a :: Type). (a -> Bool) -> Set a -> Set a
filter f = Set . PlutusTx.Prelude.filter f . unSet

{- | The resulting set can be less than the initial size
if f maps two or more different keys to the same new key.
-}
{-# INLINEABLE map #-}
map :: forall (a :: Type) (b :: Type). Ord b => (a -> b) -> Set a -> Set b
map f = fromList . PlutusTx.Prelude.map f . unSet

{-# INLINEABLE union #-}
union :: forall (a :: Type). Ord a => Set a -> Set a -> Set a
union x y = foldr insert x (toList y)

PlutusTx.makeLift ''Set

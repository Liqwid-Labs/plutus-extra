{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas
  -fno-strictness
  -fno-specialize #-}

module PlutusTx.Set (
  Set,
  insert,
  size,
  toList,
  fromList,
  null,
  member,
  delete,
  union,
  singleton,
  empty,
  all,
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)
import GHC.Generics (Generic)
import Prelude qualified

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
import PlutusTx qualified (makeIsDataIndexed, makeLift)
import PlutusTx.Prelude hiding (all, foldMap, null, toList)
import PlutusTx.Prelude qualified

--------------------------------------------------------------------------------

newtype Set (a :: Type) = Set {unSet :: [a]}
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving newtype (ToJSON, FromJSON)

-- TODO: these Eq/Ord instances perform very misleadingly... :/
deriving newtype instance Eq a => Eq (Set a)
deriving newtype instance Ord a => Ord (Set a)

instance Functor Set where
  {-# INLINEABLE fmap #-}
  fmap f = Set . fmap f . unSet

instance Eq a => Semigroup (Set a) where
  {-# INLINEABLE (<>) #-}
  (<>) = union

instance Eq a => Monoid (Set a) where
  {-# INLINEABLE mempty #-}
  mempty = empty

instance Foldable Set where
  {-# INLINEABLE foldMap #-}
  foldMap :: forall (o :: Type) (a :: Type). Monoid o => (a -> o) -> Set a -> o
  foldMap f = mconcat . fmap f . toList

instance Traversable Set where
  {-# INLINEABLE traverse #-}
  traverse f = fmap Set . traverse f . toList

{-# INLINEABLE empty #-}
empty :: forall (a :: Type). Eq a => Set a
empty = fromList []

{-# INLINEABLE singleton #-}
singleton :: forall (a :: Type). Eq a => a -> Set a
singleton = fromList . (: [])

{-# INLINEABLE insert #-}
insert :: forall (a :: Type). Eq a => a -> Set a -> Set a
insert v (Set s)
  | elem v s = Set s
  | otherwise = Set (v : s)

{-# INLINEABLE toList #-}
toList :: forall (a :: Type). Set a -> [a]
toList = unSet

{-# INLINEABLE fromList #-}
fromList :: forall (a :: Type). Eq a => [a] -> Set a
fromList = Set . nub

{-# INLINEABLE member #-}
member :: forall (a :: Type). Eq a => a -> Set a -> Bool
member n = elem n . unSet

{-# INLINEABLE delete #-}
delete :: forall (a :: Type). Eq a => a -> Set a -> Set a
delete n = Set . filter (/= n) . unSet

{-# INLINEABLE union #-}
union :: forall (a :: Type). Eq a => Set a -> Set a -> Set a
union x y = fromList $ toList x <> toList y

{-# INLINEABLE null #-}
null :: forall (a :: Type). Set a -> Bool
null = PlutusTx.Prelude.null . unSet

{-# INLINEABLE all #-}
all :: forall (a :: Type). (a -> Bool) -> Set a -> Bool
all predicate = PlutusTx.Prelude.all predicate . toList

{-# INLINEABLE size #-}
size :: forall (a :: Type). Set a -> Integer
size = length . toList

PlutusTx.makeLift ''Set
PlutusTx.makeIsDataIndexed ''Set [('Set, 0)]

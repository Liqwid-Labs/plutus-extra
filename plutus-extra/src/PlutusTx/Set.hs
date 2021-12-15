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
import Data.OpenApi.Internal.Schema qualified as OpenApi
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  functionMap,
 )

--------------------------------------------------------------------------------

import PlutusTx qualified (makeLift)
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Prelude hiding (all, filter, foldMap, map, null, toList)
import PlutusTx.Prelude qualified

--------------------------------------------------------------------------------

-- | A set of unique values @a@.
newtype Set (a :: Type) = Set {unSet :: [a]}
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving newtype (ToJSON)

deriving newtype instance Eq a => Eq (Set a)
deriving newtype instance Ord a => Ord (Set a)

-- | @since 4.0
deriving newtype instance OpenApi.ToSchema a => OpenApi.ToSchema (Set a)

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

instance (ToData a) => ToData (Set a) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData = toBuiltinData . toList

instance (Ord a, FromData a) => FromData (Set a) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = PlutusTx.Prelude.fmap fromList . fromBuiltinData

instance (Ord a, UnsafeFromData a) => UnsafeFromData (Set a) where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = fromList . unsafeFromBuiltinData

-- | @since 4.0
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
  arbitrary = Prelude.fmap fromList arbitrary
  shrink = fmap fromList . shrink . toList

-- | @since 4.0
instance (CoArbitrary a) => CoArbitrary (Set a) where
  coarbitrary = coarbitrary . toList

-- | @since 4.0
instance (Function a, Ord a) => Function (Set a) where
  function = functionMap toList fromList

{-# INLINEABLE empty #-}

-- | Construct a empty set
empty :: forall (a :: Type). Ord a => Set a
empty = fromList []

{-# INLINEABLE singleton #-}

-- | Construct a singleton set from a single element.
singleton :: forall (a :: Type). Ord a => a -> Set a
singleton = fromList . (: [])

{-# INLINEABLE insert #-}

-- | Insert an element in a set.
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

-- | Convert the set to a list of elements.
toList :: forall (a :: Type). Set a -> [a]
toList = unSet

{-# INLINEABLE fromList #-}

-- | Create a set from a list of elements.
fromList :: forall (a :: Type). Ord a => [a] -> Set a
fromList = foldr insert (Set [])

{-# INLINEABLE null #-}

-- | Is the set empty?
null :: forall (a :: Type). Set a -> Bool
null = PlutusTx.Prelude.null . unSet

{-# INLINEABLE member #-}

-- | Is the element in the set?
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

-- | The number of elements in the set.
size :: forall (a :: Type). Set a -> Integer
size = length . toList

{-# INLINEABLE all #-}

-- | Do all elements in the set satisfy the predicate?
all :: forall (a :: Type). (a -> Bool) -> Set a -> Bool
all predicate = PlutusTx.Prelude.all predicate . toList

{-# INLINEABLE delete #-}

-- | Remove an element from the set.
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

-- | Filter all elements that satisfy the predicate.
filter :: forall (a :: Type). (a -> Bool) -> Set a -> Set a
filter f = Set . PlutusTx.Prelude.filter f . unSet

{-# INLINEABLE map #-}

{- | The resulting set can be less than the initial size
if f maps two or more different keys to the same new key.
-}
map :: forall (a :: Type) (b :: Type). Ord b => (a -> b) -> Set a -> Set b
map f = fromList . PlutusTx.Prelude.map f . unSet

{-# INLINEABLE union #-}

-- | The union of two sets.
union :: forall (a :: Type). Ord a => Set a -> Set a -> Set a
union x y = foldr insert x (toList y)

PlutusTx.makeLift ''Set

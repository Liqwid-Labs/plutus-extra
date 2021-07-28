{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
-- Prevent unboxing, which the plugin can't deal with
{-# OPTIONS_GHC -fno-strictness #-}

-- | A map represented as an "association list" of key-value pairs.
module PlutusTx.UniqueMap (
  Map,
  singleton,
  empty,
  null,
  fromList,
  toList,
  keys,
  elems,
  lookup,
  member,
  insert,
  delete,
  union,
  unionWith,
  filter,
  mapWithKey,
  mapMaybe,
  mapMaybeWithKey,
  all,
  mapThese,
  alter,
) where

------------------------------------------------------------------------------
import Control.DeepSeq (NFData)
import Data.Aeson qualified as JSON
import Data.Bifunctor as Data
import Data.Kind (Type)
import GHC.Generics (Generic)
import Prelude qualified as Haskell

--------------------------------------------------
import PlutusTx.IsData
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude hiding (all, filter, mapMaybe, null, toList)
import PlutusTx.Prelude qualified as P
import PlutusTx.These

{- HLINT ignore "Use newtype instead of data" -}

-- | A 'Map' of key-value pairs.
newtype Map (k :: Type) (v :: Type) = Map {unMap :: [(k, v)]}
  deriving stock (Generic, Haskell.Eq, Haskell.Show)
  deriving newtype (Eq, Ord, IsData, NFData)

instance Functor (Map k) where
  {-# INLINEABLE fmap #-}
  fmap f (Map mp) =
    let go [] = []
        go ((c, i) : xs) = (c, f i) : go xs
     in Map (go mp)

instance Foldable (Map k) where
  {-# INLINEABLE foldMap #-}
  foldMap f (Map mp) =
    let go [] = mempty
        go ((_, i) : xs) = f i <> go xs
     in go mp

instance Traversable (Map k) where
  {-# INLINEABLE traverse #-}
  traverse f (Map mp) =
    let go [] = pure []
        go ((c, i) : xs) = (\i' xs' -> (c, i') : xs') <$> f i <*> go xs
     in Map <$> go mp

-- This is the "better" instance for Maps that various people
-- have suggested, which merges conflicting entries with
-- the underlying semigroup for values.
instance (Eq k, Semigroup v) => Semigroup (Map k v) where
  (<>) = unionWith (<>)

instance (Eq k, Semigroup v) => Monoid (Map k v) where
  mempty = empty

-- Orphan instances for 'Map' to make this work
instance (JSON.ToJSON v, JSON.ToJSON k) => JSON.ToJSON (Map k v) where
  toJSON = JSON.toJSON . toList

instance (JSON.FromJSON v, JSON.FromJSON k, Eq k) => JSON.FromJSON (Map k v) where
  parseJSON v = fromList Haskell.<$> JSON.parseJSON v

{-# INLINEABLE fromList #-}

-- | Create a `Map` from a list of key and value pairs.
fromList :: forall (k :: Type) (v :: Type). (Eq k) => [(k, v)] -> Map k v
fromList l = Map (nubBy (\(x, _) (y, _) -> x == y) l)

{-# INLINEABLE toList #-}

-- | Convert the `Map` to a list of key and value pairs.
toList :: forall (k :: Type) (v :: Type). Map k v -> [(k, v)]
toList (Map l) = l

{-# INLINEABLE lookup #-}

-- | Find an entry in a 'Map'.
lookup :: forall (k :: Type) (v :: Type). (Eq k) => k -> Map k v -> Maybe v
lookup c (Map xs) =
  let go :: [(k, v)] -> Maybe v
      go [] = Nothing
      go ((c', i) : xs') = if c' == c then Just i else go xs'
   in go xs

{-# INLINEABLE member #-}

-- | Is the key a member of the map?
member :: forall (k :: Type) (v :: Type). (Eq k) => k -> Map k v -> Bool
member k m = isJust (lookup k m)

{-# INLINEABLE insert #-}

-- | Insert a new key and value in a `Map`.
insert :: forall (k :: Type) (v :: Type). Eq k => k -> v -> Map k v -> Map k v
insert k v m = unionWith (\_ b -> b) m (fromList [(k, v)])

{-# INLINEABLE delete #-}

-- | Remove a key and value from a `Map` for given key.
delete :: forall (k :: Type) (v :: Type). (Eq k) => k -> Map k v -> Map k v
delete key (Map ls) = Map (go ls)
  where
    go [] = []
    go ((k, v) : rest)
      | k == key = rest
      | otherwise = (k, v) : go rest

{-# INLINEABLE keys #-}

-- | The keys of a 'Map'.
keys :: forall (k :: Type) (v :: Type). Map k v -> [k]
keys (Map xs) = P.fmap (\(k, _ :: v) -> k) xs

{-# INLINEABLE union #-}

-- | Combine two 'Map's.
union ::
  forall (k :: Type) (v :: Type) (r :: Type).
  (Eq k) =>
  Map k v ->
  Map k r ->
  Map k (These v r)
union (Map ls) (Map rs) =
  let f :: v -> Maybe r -> These v r
      f a b' = case b' of
        Nothing -> This a
        Just b -> These a b

      ls' :: [(k, These v r)]
      ls' = P.fmap (\(c, i) -> (c, f i (lookup c (Map rs)))) ls

      rs' :: [(k, r)]
      rs' = P.filter (\(c, _) -> not (any (\(c', _) -> c' == c) ls)) rs

      rs'' :: [(k, These v r)]
      rs'' = P.fmap (Data.second That) rs'
   in Map (ls' ++ rs'')

{-# INLINEABLE unionWith #-}

-- | Combine two 'Map's with the given combination function.
unionWith ::
  forall (k :: Type) (a :: Type).
  (Eq k) =>
  (a -> a -> a) ->
  Map k a ->
  Map k a ->
  Map k a
unionWith merge (Map ls) (Map rs) =
  let f :: a -> Maybe a -> a
      f a b' = case b' of
        Nothing -> a
        Just b -> merge a b

      ls' :: [(k, a)]
      ls' = P.fmap (\(c, i) -> (c, f i (lookup c (Map rs)))) ls

      rs' :: [(k, a)]
      rs' = P.filter (\(c, _) -> not (any (\(c', _) -> c' == c) ls)) rs
   in Map (ls' ++ rs')

{-# INLINEABLE all #-}

-- | See 'Data.Map.all'
all :: forall (k :: Type) (v :: Type). (v -> Bool) -> Map k v -> Bool
all p (Map mps) =
  let go xs = case xs of
        [] -> True
        (_ :: k, x) : xs' -> p x && go xs'
   in go mps

-- | A version of 'Data.Map.Lazy.mapEither' that works with 'These'.
mapThese ::
  forall (a :: Type) (b :: Type) (k :: Type) (v :: Type).
  (v -> These a b) ->
  Map k v ->
  (Map k a, Map k b)
mapThese f mps = (Map mpl, Map mpr)
  where
    (mpl, mpr) = P.foldr f' ([], []) mps'
    Map mps' = fmap f mps
    f' (k, v) (as, bs) = case v of
      This a -> ((k, a) : as, bs)
      That b -> (as, (k, b) : bs)
      These a b -> ((k, a) : as, (k, b) : bs)

-- | A singleton map.
singleton :: forall (k :: Type) (v :: Type). k -> v -> Map k v
singleton c i = Map [(c, i)]

{-# INLINEABLE empty #-}

-- | An empty 'Map'.
empty :: forall (k :: Type) (v :: Type). Map k v
empty = Map ([] :: [(k, v)])

{-# INLINEABLE null #-}

-- | Is the map empty?
null :: forall (k :: Type) (v :: Type). Map k v -> Bool
null = P.null . unMap

{-# INLINEABLE filter #-}

-- | Filter all values that satisfy the predicate.
filter :: forall (k :: Type) (v :: Type). (v -> Bool) -> Map k v -> Map k v
filter f (Map m) = Map $ P.filter (f . snd) m

{-# INLINEABLE elems #-}

-- | Return all elements of the map in the ascending order of their keys.
elems :: forall (k :: Type) (v :: Type). Map k v -> [v]
elems (Map xs) = P.fmap (\(_ :: k, v) -> v) xs

{-# INLINEABLE mapWithKey #-}

-- | Map a function over all values in the map.
mapWithKey ::
  forall (k :: Type) (a :: Type) (b :: Type).
  (k -> a -> b) ->
  Map k a ->
  Map k b
mapWithKey f (Map xs) = Map $ fmap (\(k, v) -> (k, f k v)) xs

{-# INLINEABLE mapMaybe #-}

-- | Map keys\/values and collect the 'Just' results.
mapMaybe ::
  forall (k :: Type) (a :: Type) (b :: Type).
  (a -> Maybe b) ->
  Map k a ->
  Map k b
mapMaybe f (Map xs) = Map $ P.mapMaybe (\(k, v) -> (k,) <$> f v) xs

{-# INLINEABLE mapMaybeWithKey #-}

-- | Map keys\/values and collect the 'Just' results.
mapMaybeWithKey ::
  forall (k :: Type) (a :: Type) (b :: Type).
  (k -> a -> Maybe b) ->
  Map k a ->
  Map k b
mapMaybeWithKey f (Map xs) = Map $ P.mapMaybe (\(k, v) -> (k,) <$> f k v) xs

-- | Alter a value that may or may not already exist in a map given a key
alter ::
  forall (k :: Type) (v :: Type).
  Eq k =>
  k ->
  (Maybe v -> Maybe v) ->
  Map k v ->
  Map k v
alter k f m =
  case f (lookup k m) of
    Nothing ->
      delete k m
    Just v ->
      insert k v m

makeLift ''Map

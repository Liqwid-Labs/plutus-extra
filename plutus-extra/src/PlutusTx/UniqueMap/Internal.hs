{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

{- | An association data structure, suitable for on-chain use, which preserves
 the uniqueness of its keys.

 This module is designed for importing qualified. The interface is based on
 'PlutusTx.AssocMap', but with less boolean blindness.

 = Warning

 This is an internal module. It should not be used unless you need access to
 the internals of 'UniqueMap' for some reason. Its main purpose is as a
 @testlib@ helper to allow us to define instances needed for testing
 directly, as opposed to using the API, which would create a cycle of trust.
 For regular use, prefer 'PlutusTx.UniqueMap' if at all possible.

 = Note

 All time complexities assume \(\Theta(1)\) order comparisons for keys.
-}
module PlutusTx.UniqueMap.Internal (
  -- * Types
  UniqueMap (..),
  Inclusion (..),

  -- * Construction
  empty,
  singleton,
  fromList,

  -- * Queries
  size,
  lookup,
  findMin,
  findMax,
  inclusion,

  -- * Updates
  insert,
  delete,
  alter,
  --   alterF,
  mapWithKey,
  PlutusTx.UniqueMap.Internal.mapMaybe,
  mapMaybeWithKey,

  -- * Combination
  union,
  unionWith,

  -- * Conversion
  PlutusTx.UniqueMap.Internal.toList,
  keys,
  elems,
) where

import Control.Monad (foldM)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value,
  withArray,
 )
import Data.Aeson.Types (Parser)
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import PlutusTx.Builtins (matchData, mkMap, unsafeDataAsMap)
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude hiding (toList)
import PlutusTx.These (These (That, These, This))
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  Gen,
  functionMap,
  sized,
 )
import Prelude qualified as P

-- | An ordered key-value dictionary which ensures key uniqueness.
newtype UniqueMap (k :: Type) (v :: Type)
  = UniqueMap [(k, v)]
  deriving stock (P.Show)
  deriving newtype (Eq, Ord, P.Eq)

instance Functor (UniqueMap k) where
  {-# INLINEABLE fmap #-}
  fmap f = mapWithKey (const f)

instance Foldable (UniqueMap k) where
  {-# INLINEABLE foldMap #-}
  foldMap f (UniqueMap um) = foldMap (f . snd) um

instance Traversable (UniqueMap k) where
  {-# INLINEABLE traverse #-}
  traverse f (UniqueMap um) = UniqueMap <$> traverse (traverse f) um

instance (Ord k, Semigroup v) => Semigroup (UniqueMap k v) where
  {-# INLINEABLE (<>) #-}
  (<>) = unionWith (<>)

instance (Ord k, Semigroup v) => Monoid (UniqueMap k v) where
  {-# INLINEABLE mempty #-}
  mempty = empty

-- This instance is designed to re-use the @Map@ encoding for @Data@. This
-- ensures uniqueness, although we lose ordering. Not much we can do about it
-- though.
instance (ToData k, ToData v) => ToData (UniqueMap k v) where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (UniqueMap um) = mkMap . fmap go $ um
    where
      go :: (k, v) -> (BuiltinData, BuiltinData)
      go (key, val) = (toBuiltinData key, toBuiltinData val)

-- We verify two things when we decode:
--

-- * That the keys are unique; and

-- * That they're in the correct order.

--
-- We reject if either is violated.

-- Because we share a representation with Map, we cannot assume order.
-- Uniqueness is still checkable (and should be maintained).
instance (FromData k, FromData v, Ord k) => FromData (UniqueMap k v) where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData d =
    matchData
      d
      (\_ _ -> Nothing)
      (\ell -> traverse decode ell >>= (sequenceA . foldr go empty))
      (const Nothing)
      (const Nothing)
      (const Nothing)
    where
      decode :: (BuiltinData, BuiltinData) -> Maybe (k, v)
      decode (k', v') = (,) <$> fromBuiltinData k' <*> fromBuiltinData v'
      go :: (k, v) -> UniqueMap k (Maybe v) -> UniqueMap k (Maybe v)
      go (key, val) = alter key (go2 val)
      go2 :: v -> Maybe (Maybe v) -> Maybe (Maybe v)
      go2 val = \case
        -- No mapping for this key, insert as Just.
        Nothing -> pure . pure $ val
        -- Either there was a duplicate insertion, or we are making a duplicate
        -- insertion. Either way, we mark it as such.
        Just _ -> Just Nothing

-- The same rules are used here, except that we error instead of returning
-- 'Nothing'.
instance
  (UnsafeFromData k, UnsafeFromData v, Ord k) =>
  UnsafeFromData (UniqueMap k v)
  where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = foldr go empty . unsafeDataAsMap
    where
      go :: (BuiltinData, BuiltinData) -> UniqueMap k v -> UniqueMap k v
      go (k', v') acc =
        let key = unsafeFromBuiltinData k'
            value = unsafeFromBuiltinData v'
         in alter key (go2 value) acc
      go2 :: v -> Maybe v -> Maybe v
      go2 val = \case
        Nothing -> Just val
        Just _ -> error . trace complaint $ ()
      complaint :: BuiltinString
      complaint = "Duplicate key found when decoding UniqueMap from BuiltinData"

instance (ToJSON k, ToJSON v) => ToJSON (UniqueMap k v) where
  {-# INLINEABLE toJSON #-}
  toJSON = toJSON . PlutusTx.UniqueMap.Internal.toList

instance (FromJSON k, FromJSON v, Ord k) => FromJSON (UniqueMap k v) where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withArray "UniqueMap" go
    where
      go :: Vector Value -> Parser (UniqueMap k v)
      go v = case Vector.uncons v of
        Nothing -> P.pure empty
        Just (val, vals) -> do
          (key, value) <- parseJSON val
          snd P.<$> foldM go2 (key, singleton key value) vals
      go2 :: (k, UniqueMap k v) -> Value -> Parser (k, UniqueMap k v)
      go2 (lastMax, acc) val = do
        (key, value) <- parseJSON val
        case compare lastMax key of
          LT -> P.pure (key, insert key value acc)
          EQ -> P.fail "Duplicate keys"
          GT -> P.fail "Keys are out of order"

-- | @since 4.0
instance (Arbitrary k, Arbitrary v, Ord k) => Arbitrary (UniqueMap k v) where
  arbitrary = do
    ks <- sized uniqueListOf
    UniqueMap P.<$> P.traverse (\key -> (key,) P.<$> arbitrary) ks
  shrink (UniqueMap um) = do
    um' <- shrink um
    P.pure $ fromList um'

-- | @since 4.0
instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (UniqueMap k v) where
  coarbitrary = coarbitrary . toList

-- | @since 4.0
instance (Function k, Function v, Ord k) => Function (UniqueMap k v) where
  function = functionMap toList fromList

-- Needed, because otherwise we can do dirty order-breaking coercions.
type role UniqueMap nominal representational

{- | A type describing the relationship between two arbitrary maps regarding
 inclusion. Used for the results of the 'inclusion' function.
-}
data Inclusion
  = -- | Neither map is a submap of the other.
    Disjoint
  | -- | The left map is a submap of the right.
    LeftIsSubmap
  | -- | The right map is a submap of the left.
    RightIsSubmap
  | -- | The two maps are identical.
    Identical

{- | The empty map.

 /Complexity:/ \(\Theta(1)\)
-}
{-# INLINEABLE empty #-}
empty :: forall (k :: Type) (v :: Type). UniqueMap k v
empty = UniqueMap []

{- | Single-pairing map.

 /Complexity:/ \(\Theta(1)\)
-}
{-# INLINEABLE singleton #-}
singleton :: forall (k :: Type) (v :: Type). k -> v -> UniqueMap k v
singleton key val = UniqueMap [(key, val)]

{- | Construct from a list of pairings. If two pairings have identical keys,
 they will be \'collapsed\' into a single entry, using the first value we saw.

 /Complexity:/ \(\Theta(n)\)
-}
{-# INLINEABLE fromList #-}
fromList ::
  forall (k :: Type) (v :: Type).
  (Ord k) =>
  [(k, v)] ->
  UniqueMap k v
fromList = foldr (\(key, val) acc -> insert key val acc) empty

{- | Get the number of mappings in the map.

 /Complexity:/ \(\Theta(n)\)
-}
{-# INLINEABLE size #-}
size ::
  forall (k :: Type) (v :: Type).
  UniqueMap k v ->
  Integer
size (UniqueMap um) = length um

{- | Look up a key in the map.

 /Complexity:/ \(\Theta(n)\)
-}
{-# INLINEABLE lookup #-}
lookup ::
  forall (k :: Type) (v :: Type).
  (Ord k) =>
  k ->
  UniqueMap k v ->
  Maybe v
lookup key (UniqueMap um) = go um
  where
    go :: [(k, v)] -> Maybe v
    go = \case
      [] -> Nothing
      ((key', val') : xs) -> case compare key key' of
        LT -> Nothing -- overshot
        EQ -> pure val'
        GT -> go xs -- keep searching

{- | Find the mapping with the smallest key, if it exists. This can be used as a
 non-blind test for emptiness.

 /Complexity:/ \(\Theta(1)\)
-}
{-# INLINEABLE findMin #-}
findMin :: forall (k :: Type) (v :: Type). UniqueMap k v -> Maybe (k, v)
findMin (UniqueMap um) = case um of
  [] -> Nothing
  (x : _) -> Just x

{- | Find the mapping with the largest key, if it exists. This can be used as a
 non-blind test for emptiness.

 /Complexity:/ \(\Theta(n)\)
-}
{-# INLINEABLE findMax #-}
findMax :: forall (k :: Type) (v :: Type). UniqueMap k v -> Maybe (k, v)
findMax (UniqueMap um) = go um
  where
    go :: [(k, v)] -> Maybe (k, v)
    go = \case
      [] -> Nothing
      [x] -> pure x
      (_ : xs) -> go xs

{- | Check the inclusion status of two maps.

 We consider two maps identical when they have the same key-value pairings (as
 per the 'Eq' instance).

 We consider a map @m@ a submap of map @m'@ if @m@ and @m' are not identical
 and, for any key @key@:

 * If @key@ is present in @m@, it is also present in @m'@; and
 * The value for @key@ is the same in both @m@ and @m'@.

 Otherwise, @m@ and @m'@ are disjoint.

 /Complexity: \(\Theta( )\)
-}
inclusion ::
  forall (k :: Type) (v :: Type).
  (Ord k, Eq v) =>
  UniqueMap k v ->
  UniqueMap k v ->
  Inclusion
inclusion um = foldr go Identical . union um
  where
    go :: These v v -> Inclusion -> Inclusion
    go val = \case
      Disjoint -> Disjoint
      LeftIsSubmap -> case val of
        This {} -> Disjoint
        That {} -> LeftIsSubmap
        These l r -> if l == r then LeftIsSubmap else Disjoint
      RightIsSubmap -> case val of
        This {} -> RightIsSubmap
        That {} -> Disjoint
        These l r -> if l == r then RightIsSubmap else Disjoint
      Identical -> case val of
        This {} -> RightIsSubmap
        That {} -> LeftIsSubmap
        These l r -> if l == r then Identical else Disjoint

{- | Insert a key-value pair into the map. If this key is already paired to a
 value, the new key will overwrite it.

 /Complexity:/ \(\Theta(n)\)
-}
{-# INLINEABLE insert #-}
insert ::
  forall (k :: Type) (v :: Type).
  (Ord k) =>
  k ->
  v ->
  UniqueMap k v ->
  UniqueMap k v
insert key val (UniqueMap um) = UniqueMap . go $ um
  where
    go :: [(k, v)] -> [(k, v)]
    go = \case
      [] -> [(key, val)]
      [(k', v')] -> case compare key k' of
        LT -> [(key, val), (k', v')]
        EQ -> [(key, val)]
        GT -> [(k', v'), (key, val)]
      ell@((k1, v1) : (k2, v2) : xs) -> case compare key k1 of
        LT -> (key, val) : ell
        EQ -> (key, val) : (k2, v2) : xs
        GT ->
          (k1, v1) : case compare key k2 of
            LT -> (key, val) : (k2, v2) : xs
            EQ -> (key, val) : xs
            GT -> (k2, v2) : go xs

{- | Remove a key-value pair from the map if it exists.

 /Complexity:/ \(\Theta(n)\)
-}
{-# INLINEABLE delete #-}
delete ::
  forall (k :: Type) (v :: Type).
  (Ord k) =>
  k ->
  UniqueMap k v ->
  UniqueMap k v
delete key (UniqueMap um) = UniqueMap . go $ um
  where
    go :: [(k, v)] -> [(k, v)]
    go = \case
      [] -> []
      ell@((k', v') : xs) -> case compare key k' of
        LT -> ell -- Not there to be removed
        EQ -> xs
        GT -> (k', v') : go xs -- Continue

{- | Alter a value at a given key, whether it exists in the map or not. This can
 insert, modify or delete a value, depending on the function.

 /Complexity:/ \(\Theta(n)\) assuming function is \(\Theta(1)\)
-}
{-# INLINEABLE alter #-}
alter ::
  forall (k :: Type) (v :: Type).
  (Ord k) =>
  k ->
  (Maybe v -> Maybe v) ->
  UniqueMap k v ->
  UniqueMap k v
alter key f (UniqueMap um) = UniqueMap . go $ um
  where
    go :: [(k, v)] -> [(k, v)]
    go = \case
      [] -> case f Nothing of
        Nothing -> []
        Just val -> [(key, val)]
      ell@[(k', v')] -> case compare key k' of
        LT -> case f Nothing of
          Nothing -> ell
          Just val -> (key, val) : ell
        EQ -> case f (Just v') of
          Nothing -> []
          Just val -> [(k', val)]
        GT -> case f Nothing of
          Nothing -> ell
          Just val -> [(k', v'), (key, val)]
      ell@((k1, v1) : (k2, v2) : ps) -> case compare key k1 of
        LT -> case f Nothing of
          Nothing -> ell
          Just val -> (key, val) : ell
        EQ -> case f (Just v1) of
          Nothing -> (k2, v2) : ps
          Just val -> (k1, val) : (k2, v2) : ps
        GT ->
          (k1, v1) : case compare key k2 of
            LT -> case f Nothing of
              Nothing -> (k2, v2) : ps
              Just val -> (key, val) : (k2, v2) : ps
            EQ -> case f (Just v2) of
              Nothing -> ps
              Just val -> (k2, val) : ps
            GT -> (k2, v2) : go ps

{- | Given a transformation function for the value that uses the key, transform
 all values in the map.

 /Complexity:/ \(\Theta(n)\) assuming function is \(\Theta(1)\)
-}
mapWithKey ::
  forall (k :: Type) (v :: Type) (u :: Type).
  (k -> v -> u) ->
  UniqueMap k v ->
  UniqueMap k u
mapWithKey f (UniqueMap um) = UniqueMap . fmap go $ um
  where
    go :: (k, v) -> (k, u)
    go (key, val) = (key, f key val)

{- | Given a tranformation function that might fail, transform all values in the
 map, throwing out those on which the transformation fails.

 /Complexity:/ \(\Theta(n)\) assuming transformation is \(\Theta(1)\)
-}
{-# INLINEABLE mapMaybe #-}
mapMaybe ::
  forall (k :: Type) (v :: Type) (u :: Type).
  (v -> Maybe u) ->
  UniqueMap k v ->
  UniqueMap k u
mapMaybe f = mapMaybeWithKey (const f)

{- | As 'PlutusTx.UniqueMap.Internal.mapMaybe', but the transformation can also make use
 of the key.

 /Complexity:/ \(\Theta(n)\) assuming transformation is \(\Theta(1)\)
-}
{-# INLINEABLE mapMaybeWithKey #-}
mapMaybeWithKey ::
  forall (k :: Type) (v :: Type) (u :: Type).
  (k -> v -> Maybe u) ->
  UniqueMap k v ->
  UniqueMap k u
mapMaybeWithKey f (UniqueMap um) = UniqueMap . go $ um
  where
    go :: [(k, v)] -> [(k, u)]
    go = \case
      [] -> []
      ((key, val) : xs) -> case f key val of
        Nothing -> go xs
        Just val' -> (key, val') : go xs

{- | Combines all pairings in both maps using 'These'. For any given
 key, if there is a corresponding value in the first map but not the second,
 it will be a 'This'; if there is a corresponding value in the second map but
 not the first, it will be a 'That'; otherwise, it will be a 'These'.

 /Complexity:/ \(\Theta(n + m)\)
-}
{-# INLINEABLE union #-}
union ::
  forall (k :: Type) (v :: Type) (u :: Type).
  (Ord k) =>
  UniqueMap k v ->
  UniqueMap k u ->
  UniqueMap k (These v u)
union (UniqueMap um) (UniqueMap um') = UniqueMap . go um $ um'
  where
    go :: [(k, v)] -> [(k, u)] -> [(k, These v u)]
    go ell ell' = case ell of
      [] -> fmap (fmap That) ell'
      ((kv, vv) : kvs) -> case ell' of
        [] -> fmap (fmap This) ell
        ((ku, vu) : kus) -> case compare kv ku of
          LT -> (kv, This vv) : go kvs ell'
          EQ -> (kv, These vv vu) : go kvs kus
          GT -> (ku, That vu) : go ell kus

{- | As 'union', except that same-key pairings are resolved using the provided
 combining function on values.

 /Complexity:/ \(\Theta(n + m)\) assuming function is \(\Theta(1)\)
-}
{-# INLINEABLE unionWith #-}
unionWith ::
  forall (k :: Type) (v :: Type).
  (Ord k) =>
  (v -> v -> v) ->
  UniqueMap k v ->
  UniqueMap k v ->
  UniqueMap k v
unionWith f (UniqueMap um) (UniqueMap um') = UniqueMap . go um $ um'
  where
    go :: [(k, v)] -> [(k, v)] -> [(k, v)]
    go ell ell' = case ell of
      [] -> ell'
      ((kl, vl) : ls) -> case ell' of
        [] -> ell
        ((kl', vl') : ls') -> case compare kl kl' of
          LT -> (kl, vl) : go ls ell'
          EQ -> (kl, f vl vl') : go ls ls'
          GT -> (kl', vl') : go ell ls'

{- | Converts to a list, ordered by keys.

 /Complexity:/ \(\Theta(1)\)
-}
{-# INLINEABLE toList #-}
toList :: forall (k :: Type) (v :: Type). UniqueMap k v -> [(k, v)]
toList (UniqueMap um) = um

{- | Converts to an ordered list of keys.

 /Complexity:/ \(\Theta(n)\)
-}
{-# INLINEABLE keys #-}
keys :: forall (k :: Type) (v :: Type). UniqueMap k v -> [k]
keys = fmap fst . PlutusTx.UniqueMap.Internal.toList

{- | Converts to a list of values, ordered according to their corresponding
 keys.

 /Complexity:/ \(\Theta(n)\)
-}
{-# INLINEABLE elems #-}
elems :: forall (k :: Type) (v :: Type). UniqueMap k v -> [v]
elems = fmap snd . PlutusTx.UniqueMap.Internal.toList

-- Helpers

-- | Generates a UniqueList of the given length.
uniqueListOf ::
  forall (a :: Type).
  (Arbitrary a, Ord a) =>
  P.Int ->
  Gen [a]
uniqueListOf len
  | len P.== 0 = P.pure []
  | len P.== 1 = P.fmap (: []) arbitrary
  | otherwise = do
    let leftLen = len `P.quot` 2
    let rightLen = len P.- leftLen
    leftHalf <- uniqueListOf leftLen
    rightHalf <- uniqueListOf rightLen
    P.pure . merge leftHalf $ rightHalf

-- This is a _monotonic_ merge; identical keys will be combined together instead
-- of being put side-by-side.
merge :: forall (a :: Type). (Ord a) => [a] -> [a] -> [a]
merge xs ys = case xs of
  [] -> ys
  (x : xs') -> case ys of
    [] -> xs
    (y : ys') -> case compare x y of
      LT -> x : merge xs' ys
      EQ -> x : merge xs' ys'
      GT -> y : merge xs ys'

makeLift ''UniqueMap

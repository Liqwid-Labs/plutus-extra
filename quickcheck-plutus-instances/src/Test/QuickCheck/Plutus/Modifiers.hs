{- | Module: Test.QuickCheck.Plutus.Modifiers
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Several newtype wrappers for more specific QuickCheck functionality involving
 Plutus types. Similar in spirit to the module of the same name in QuickCheck.
-}
module Test.QuickCheck.Plutus.Modifiers (
  UniqueList (..),
  UniqueKeys (..),
  NonNegative (..),
  NonZero (..),
  uniqueListOf,
) where

import Control.Monad (guard)
import Data.Bitraversable (bitraverse)
import Data.Kind (Type)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary (coarbitrary),
 )
import Test.QuickCheck.Function (Function (function), functionMap)
import Test.QuickCheck.Gen (
  Gen,
  chooseInt,
  sized,
  suchThat,
  variant,
 )

{- | A wrapper around (Plutus) lists that ensures elements are unique. The
 elements are also ordered ascending according to the 'PlutusTx.Ord' instance
 (due to how the implementation works).

 @since 1.1
-}
newtype UniqueList (a :: Type) = UniqueList [a]
  deriving stock
    ( -- | @since 1.1
      Show
    )
  deriving
    ( -- | @since 1.1
      Eq
    , -- | @since 1.1
      PlutusTx.Eq
    , -- | @since 1.1
      CoArbitrary
    )
    via [a]
  deriving
    ( -- | @since 1.1
      PlutusTx.Functor
    )
    via []

-- | @since 1.1
instance (Arbitrary a, PlutusTx.Ord a) => Arbitrary (UniqueList a) where
  arbitrary = sized $ \ size -> chooseInt (0, size) >>= uniqueListOf
  shrink (UniqueList xs) = UniqueList <$> (filter isSorted . shrink $ xs)

-- | @since 1.1
instance (Function a) => Function (UniqueList a) where
  function = functionMap into UniqueList
    where
      into :: UniqueList a -> [a]
      into (UniqueList xs) = xs

{- | A wrapper around 'AssocMap.Map' that ensures keys are unique.

 @since 1.1
-}
newtype UniqueKeys (k :: Type) (v :: Type) =
  UniqueKeys
  { -- | @since 1.3
    unUniqueKeys :: AssocMap.Map k v
  }
  deriving stock
    ( -- | @since 1.1
      Show
    )
  deriving
    ( -- | @since 1.1
      Eq
    , -- | @since 1.1
      PlutusTx.Eq
    )
    via (AssocMap.Map k v)
  deriving
    ( -- | @since 1.1
      PlutusTx.Functor
    )
    via (AssocMap.Map k)

-- | @since 1.1
instance
  (Arbitrary k, Arbitrary v, PlutusTx.Ord k) =>
  Arbitrary (UniqueKeys k v)
  where
  arbitrary = liftArbitrary arbitrary
  shrink = liftShrink shrink

-- | @since 1.1
instance (Arbitrary k, PlutusTx.Ord k) => Arbitrary1 (UniqueKeys k) where
  liftArbitrary :: forall (v :: Type). Gen v -> Gen (UniqueKeys k v)
  liftArbitrary genVal = do
    UniqueList ks <- arbitrary
    UniqueKeys . AssocMap.fromList <$> traverse go ks
    where
      go :: k -> Gen (k, v)
      go key = (key,) <$> genVal
  liftShrink shr (UniqueKeys aMap) = do
    let asList = AssocMap.toList aMap
    asList' <- liftShrink (bitraverse shrink shr) asList
    guard (isSortedOn fst asList')
    pure . UniqueKeys . AssocMap.fromList $ asList'

-- | @since 1.1
instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (UniqueKeys k v) where
  coarbitrary (UniqueKeys aMap) gen = do
    let asList = AssocMap.toList aMap
    case asList of
      [] -> variant (0 :: Int) gen
      (kv : kvs) -> variant (1 :: Int) . coarbitrary (kv, kvs) $ gen

-- | @since 1.1
instance (Function k, Function v) => Function (UniqueKeys k v) where
  function = functionMap into outOf
    where
      into :: UniqueKeys k v -> [(k, v)]
      into (UniqueKeys aMap) = AssocMap.toList aMap
      outOf :: [(k, v)] -> UniqueKeys k v
      outOf = UniqueKeys . AssocMap.fromList

{- | A newtype around numerical types which ensures they are not negative; that
 is, they are greater or equal to 'PlutusTx.zero'. This is a Plutus-specific
 generalization of the wrapper of the same name from QuickCheck.

 @since 1.1
-}
newtype NonNegative (a :: Type) = NonNegative a
  deriving stock
    ( -- | @since 1.1
      Show
    )
  deriving
    ( -- | @since 1.1
      Eq
    , -- | @since 1.1
      PlutusTx.Eq
    , -- | @since 1.1
      CoArbitrary
    )
    via a

-- | @since 1.1
instance
  (Arbitrary a, PlutusTx.Ord a, PlutusTx.AdditiveMonoid a) =>
  Arbitrary (NonNegative a)
  where
  arbitrary = NonNegative <$> suchThat arbitrary (PlutusTx.>= PlutusTx.zero)
  shrink (NonNegative x) = do
    x' <- shrink x
    guard (x' PlutusTx.>= PlutusTx.zero)
    pure . NonNegative $ x'

-- | @since 1.1
instance (Function a) => Function (NonNegative a) where
  function = functionMap into NonNegative
    where
      into :: NonNegative a -> a
      into (NonNegative x) = x

{- | A newtype around numerical types which ensures they are not zero; that is,
 that their value is not 'PlutusTx.zero'. This is a Plutus-specific
 generalization of the wrapper of the same name from QuickCheck.

 @since 1.2
-}
newtype NonZero (a :: Type) = NonZero a
  deriving stock
    ( -- | @since 1.2
      Show
    )
  deriving
    ( -- | @since 1.2
      Eq
    , -- | @since 1.2
      PlutusTx.Eq
    , -- | @since 1.2
      CoArbitrary
    )
    via a

-- | @since 1.2
instance
  (Arbitrary a, PlutusTx.Eq a, PlutusTx.AdditiveMonoid a) =>
  Arbitrary (NonZero a)
  where
  arbitrary = NonZero <$> suchThat arbitrary (PlutusTx./= PlutusTx.zero)
  shrink (NonZero x) = do
    x' <- shrink x
    guard (x' PlutusTx./= PlutusTx.zero)
    pure . NonZero $ x'

-- | @since 1.2
instance (Function a) => Function (NonZero a) where
  function = functionMap into NonZero
    where
      into :: NonZero a -> a
      into (NonZero x) = x

{- | Generates a UniqueList of the given length.
 
 @since 1.3
-}
uniqueListOf ::
  forall (a :: Type).
  (Arbitrary a, PlutusTx.Ord a) =>
  Int ->
  Gen (UniqueList a)
uniqueListOf size = UniqueList <$> recGen size
  where
    recGen :: Int -> Gen [a]
    recGen len
      | len == 0 = pure []
      | len == 1 = (: []) <$> arbitrary
      | otherwise = do
        let leftLen = len `quot` 2
        let rightLen = len - leftLen
        leftHalf <- recGen leftLen
        rightHalf <- recGen rightLen
        pure . merge leftHalf $ rightHalf

-- Helpers

-- This is a _monotonic_ merge; identical keys will be combined together instead
-- of being put side-by-side.
merge :: forall (a :: Type). (PlutusTx.Ord a) => [a] -> [a] -> [a]
merge xs ys = case xs of
  [] -> ys
  (x : xs') -> case ys of
    [] -> xs
    (y : ys') -> case PlutusTx.compare x y of
      LT -> x : merge xs' ys
      EQ -> x : merge xs' ys'
      GT -> y : merge xs ys'

-- This checks for a _strict_ sort; specifically, each element must be
-- _strictly_ smaller than the one after it.
isSorted :: forall (a :: Type). (PlutusTx.Ord a) => [a] -> Bool
isSorted = isSortedOn id

-- This checks for a _strict_ sort on the target; specifically, each element
-- (according to the projection) must be _strictly_ smaller than the one after
-- it.
isSortedOn ::
  forall (a :: Type) (b :: Type).
  (PlutusTx.Ord b) =>
  (a -> b) ->
  [a] ->
  Bool
isSortedOn f = \case
  [] -> True
  [_] -> True
  (x : y : xs') -> case PlutusTx.compare (f x) (f y) of
    LT -> isSortedOn f xs'
    EQ -> False
    GT -> False
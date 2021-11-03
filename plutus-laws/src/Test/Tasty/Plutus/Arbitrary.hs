{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Tasty.Plutus.Arbitrary (
  Pair (..),
  Triple (..),
  Entangled (Entangled, Disentangled),
  Entangled3 (Entangled3, Disentangled3),
) where

import Data.Kind (Type)
import Test.QuickCheck.Arbitrary (
  Arbitrary1 (liftArbitrary, liftShrink),
  arbitrary,
 )

-- | @since 1.0
data Pair (a :: Type) = Pair a a
  deriving stock
    ( -- | @since 1.0
      Functor
    , -- | @since 1.0
      Show
    )

-- | @since 1.0
instance Arbitrary1 Pair where
  liftArbitrary gen = Pair <$> gen <*> gen
  liftShrink shr (Pair x y) = Pair <$> shr x <*> shr y

-- | @since 1.0
data Triple (a :: Type) = Triple a a a
  deriving stock
    ( -- | @since 1.0
      Functor
    , -- | @since 1.0
      Show
    )

-- | @since 1.0
instance Arbitrary1 Triple where
  liftArbitrary gen = Triple <$> gen <*> gen <*> gen
  liftShrink shr (Triple x y z) =
    Triple <$> shr x <*> shr y <*> shr z

-- | @since 1.0
data Entangled (a :: Type)
  = Same a
  | PossiblyDifferent a a
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
pattern Entangled :: a -> a -> Entangled a
pattern Entangled x y <- (delta -> Just (x, y))

-- | @since 1.0
pattern Disentangled :: a -> a -> Entangled a
pattern Disentangled x y <- PossiblyDifferent x y

{-# COMPLETE Entangled, Disentangled #-}

-- | @since 1.0
instance Arbitrary1 Entangled where
  liftArbitrary gen = do
    b <- arbitrary
    x <- gen
    if b
      then pure . Same $ x
      else PossiblyDifferent x <$> gen
  liftShrink shr = \case
    Same x -> Same <$> shr x
    PossiblyDifferent x y -> PossiblyDifferent <$> shr x <*> shr y

-- | @since 1.0
data Entangled3 (a :: Type)
  = Same3 a
  | PossiblyDifferent3 a a a
  deriving stock
    ( -- | @since 1.0
      Show
    )

-- | @since 1.0
pattern Entangled3 :: a -> a -> a -> Entangled3 a
pattern Entangled3 x y z <- (delta3 -> Just (x, y, z))

-- | @since 1.0
pattern Disentangled3 :: a -> a -> a -> Entangled3 a
pattern Disentangled3 x y z = PossiblyDifferent3 x y z

{-# COMPLETE Entangled3, Disentangled3 #-}

-- | @since 1.0
instance Arbitrary1 Entangled3 where
  liftArbitrary gen = do
    b <- arbitrary
    x <- gen
    if b
      then pure . Same3 $ x
      else PossiblyDifferent3 x <$> gen <*> gen
  liftShrink shr = \case
    Same3 x -> Same3 <$> shr x
    PossiblyDifferent3 x y z ->
      PossiblyDifferent3 <$> shr x <*> shr y <*> shr z

-- Helpers

-- Helper for the view pattern on Entangled
delta ::
  forall (a :: Type).
  Entangled a ->
  Maybe (a, a)
delta = \case
  Same x -> Just (x, x)
  PossiblyDifferent _ _ -> Nothing

-- Helper for the view pattern on Entangled3
delta3 ::
  forall (a :: Type).
  Entangled3 a ->
  Maybe (a, a, a)
delta3 = \case
  Same3 x -> Just (x, x, x)
  PossiblyDifferent3 {} -> Nothing

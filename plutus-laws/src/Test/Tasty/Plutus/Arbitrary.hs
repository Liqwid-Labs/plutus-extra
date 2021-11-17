{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- |
 Module: Test.Tasty.Plutus.Arbitrary
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A collection of helper types for defining 'Arbitrary' instances.
-}
module Test.Tasty.Plutus.Arbitrary (
  Pair (..),
  Triple (..),
  Entangled (Entangled, Disentangled),
  Entangled3 (Entangled3, Disentangled3),
  PA,
) where

import Data.Kind (Type)
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck.Arbitrary (
  Arbitrary,
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary,
  arbitrary,
 )
import Test.QuickCheck.Function (Function (function), functionMap)
import Test.QuickCheck.Poly (A (A))

{- | A single-type tuple. Its 'Arbitrary1' instance makes it convenient to
 generate pairs of the same type with an explicit generator:

 > aPair :: Pair a <- liftArbitrary gen

 @since 1.0
-}
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

{- | A single-type 3-tuple. Its 'Arbitrary1' instance makes it convenient to
 generate pairs of the same type with an explicit generator:

 > aTriple :: Triple a <- liftArbitrary gen

 @since 1.0
-}
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

{- | 'Entangled' is like 'Pair', but comes in two flavours; either it contains
 two independently-generated @a@s, or two copies of the /same/ @a@.

 This is mostly designed to address issues of coverage, such as those that
 might arise from testing a symmetry law for some type class method @foo@
 (namely, \'if @x `foo` y@, then @y `foo` x@\'). Writing a property test for
 such a method naively is unlikely to be useful, especially if the type being
 tested over has large (or infinite) cardinality, as the precondition will be
 false with high probability. This creates coverage issues.

 This type ensures that we generate enough cases to test the precondition in
 such situations being true /and/ false, and that both occur roughly as often
 as each other. The type is designed carefully in that you /cannot/ manually
 construct an 'Entangled' - you can only generate one (using 'liftArbitrary',
 for example) or pattern match on it.

 = Important notes

 Using this type requires an assumption of reflexivity in whatever method or
 methods you are seeking to test (namely, identical things should behave
 identically). This is typical for pure code (in fact, it is required), but
 when effects come into play, this may fail. We cannot check for this in
 general; take care that you do.

 Additionally, if the cardinality of the type over which tests are being run
 is both finite and small, 'Entangled' will make coverage /worse/, as it will
 skew the cases towards pairs that match over pairs which don't. Ensure that
 you don't have such a situation before you use this.

 @since 1.0
-}
data Entangled (a :: Type)
  = Same a
  | PossiblyDifferent a a
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | You can use this just like a data constructor, but /only/ for pattern
 matching. If we match, we know that the two values are, in fact, the same
 value.

 > case ent of
 >    Entangled x y -> ... -- we know that x and y are the same thing

 @since 1.0
-}
pattern Entangled :: a -> a -> Entangled a
pattern Entangled x y <- (delta -> Just (x, y))

{- | You can use this just like a data constructor, but /only/ for pattern
 matching. If we match, we know that the two values were generated
 independently; they /could/ still be the same, but we don't necessarily know
 that.

 > case ent of
 >    Disentangled x y -> ... -- x and y might not be the same

 @since 1.0
-}
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

{- | Similar to 'Entangled', but for three values instead of two.

 = Note

 All caveats on the use of 'Entangled' also apply here.

 @since 1.0
-}
data Entangled3 (a :: Type)
  = Same3 a
  | PossiblyDifferent3 a a a
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | You can use this just like a data constructor, but /only/ for pattern
 matching. If we match, we know that the three values are, in fact, the same
 value.

 > case ent3 of
 >    Entangled3 x y z -> ... -- we know that x, y, z are all the same thing

 @since 1.0
-}
pattern Entangled3 :: a -> a -> a -> Entangled3 a
pattern Entangled3 x y z <- (delta3 -> Just (x, y, z))

{- | You can use this just like a data constructor, but /only/ for pattern
 matching. If we match, we know that the three values were generated
 independently; some, or all, of them /could/ be the same, but we don't
 necessarily know that.

 > case ent3 of
 >    Disentangled3 x y z -> ... -- x, y and/or z might not be the same

 @since 1.0
-}
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

{- | A newtype wrapper for testing polymorphic properties. This is mostly useful
 for function tests involving 'CoArbitrary' and 'Function', when the result
 type isn't important. This newtype also defines several instances of Plutus
 type classes (hence 'PA'), but is otherwise identical to 'A' from QuickCheck.

 @since 2.1
-}
newtype PA = PA A
  deriving
    ( -- | @since 2.1
      Eq
    , -- | @since 2.1
      Show
    , -- | @since 2.1
      Arbitrary
    , -- | @since 2.1
      CoArbitrary
    )
    via A
  deriving
    ( -- | @since 2.1
      PlutusTx.Eq
    , -- | @since 2.1
      PlutusTx.Ord
    )
    via Integer

-- | @since 2.1
instance Function PA where
  function = functionMap into PA
    where
      into :: PA -> A
      into (PA x) = x

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

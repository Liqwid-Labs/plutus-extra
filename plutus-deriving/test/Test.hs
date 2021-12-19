{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test (Foo) where

import Control.Applicative (empty)
import Data.Kind (Type)
import PlutusTx.Deriving (deriveEq)
import PlutusTx.Prelude
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary (coarbitrary),
 )
import Test.QuickCheck.Function (Function (function), functionMap)
import Test.QuickCheck.Gen (
  Gen,
  oneof,
  resize,
  sized,
  variant,
 )
import Test.QuickCheck.Plutus.Instances ()
import Prelude qualified

data Foo (a :: Type)
  = Empty
  | Mono BuiltinByteString Integer
  | Poly a
  | Recursive BuiltinByteString (Foo a)
  deriving stock (Prelude.Eq, Prelude.Show)

$(deriveEq ''Foo)

instance (Arbitrary a) => Arbitrary (Foo a) where
  arbitrary = liftArbitrary arbitrary
  shrink = liftShrink shrink

instance (CoArbitrary a) => CoArbitrary (Foo a) where
  coarbitrary foo gen = case foo of
    Empty -> variant (0 :: Prelude.Int) gen
    Mono bs i ->
      variant (1 :: Prelude.Int) . coarbitrary bs . coarbitrary i $ gen
    Poly x ->
      variant (2 :: Prelude.Int) . coarbitrary x $ gen
    Recursive bs rec ->
      variant (3 :: Prelude.Int) . coarbitrary bs . coarbitrary rec $ gen

instance (Function a) => Function (Foo a) where
  function = functionMap into outOf
    where
      into ::
        Foo a ->
        Either
          (Maybe (BuiltinByteString, Integer))
          (Either a (BuiltinByteString, Foo a))
      into = \case
        Empty -> Left Nothing
        Mono bs i -> Left . Just $ (bs, i)
        Poly x -> Right . Left $ x
        Recursive bs rec -> Right . Right $ (bs, rec)
      outOf ::
        Either
          (Maybe (BuiltinByteString, Integer))
          (Either a (BuiltinByteString, Foo a)) ->
        Foo a
      outOf = \case
        Left Nothing -> Empty
        Left (Just (bs, i)) -> Mono bs i
        Right (Left x) -> Poly x
        Right (Right (bs, rec)) -> Recursive bs rec

instance Arbitrary1 Foo where
  liftArbitrary :: forall (a :: Type). Gen a -> Gen (Foo a)
  liftArbitrary gen = sized go
    where
      go :: Prelude.Int -> Gen (Foo a)
      go i
        | i Prelude.<= 0 = oneof [anEmpty, aMono, aPoly]
        | otherwise = oneof [anEmpty, aMono, aPoly, aRecursive i]
      anEmpty :: Gen (Foo a)
      anEmpty = Prelude.pure Empty
      aMono :: Gen (Foo a)
      aMono = Mono Prelude.<$> arbitrary Prelude.<*> arbitrary
      aPoly :: Gen (Foo a)
      aPoly = Poly Prelude.<$> gen
      aRecursive :: Prelude.Int -> Gen (Foo a)
      aRecursive i = do
        bs <- arbitrary
        rec <- resize (i `Prelude.quot` 2) . liftArbitrary $ gen
        Prelude.pure . Recursive bs $ rec
  liftShrink shr = \case
    Empty -> empty
    Mono bs i -> Mono Prelude.<$> shrink bs Prelude.<*> shrink i
    Poly x -> Poly Prelude.<$> shr x
    Recursive bs rec ->
      Recursive Prelude.<$> shrink bs Prelude.<*> liftShrink shr rec

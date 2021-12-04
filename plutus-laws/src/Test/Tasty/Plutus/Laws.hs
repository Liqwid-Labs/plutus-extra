{-# LANGUAGE AllowAmbiguousTypes #-}

{- |
 Module: Test.Tasty.Plutus.Laws
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Allows for automatically testing various type class laws using QuickCheck.

 Can be used like so:

 > import Test.Tasty.Plutus.Laws (jsonLaws, dataLawsWith)
 >
 > myLawsTests :: TestTree
 > myLawsTests = testGroup "Laws" [
 >    jsonLaws @MyType,
 >    dataLawsWith myGen myShrinker
 >    ...

 = Note

 This module's API is a wrapper around @tasty-quickcheck@; thus, all options
 used by @tasty-quickcheck@ can also be used here.
-}
module Test.Tasty.Plutus.Laws (
  -- * Test API

  -- ** Serialization
  jsonLaws,
  jsonLawsWith,
  dataLaws,
  dataLawsWith,

  -- ** Plutus type classes

  -- *** Eq
  plutusEqLaws,
  plutusEqLawsWith,
  plutusEqLawsDirect,
  plutusEqLawsDirectWith,
  plutusEqLawsSubstitution,
  plutusEqLawsSubstitutionWith,
  plutusEqLawsSubstitutionDirect,
  plutusEqLawsSubstitutionDirectWith,

  -- *** Ord
  plutusOrdLaws,
  plutusOrdLawsWith,
  plutusOrdLawsDirect,
  plutusOrdLawsDirectWith,

  -- *** Semigroup and Monoid
  plutusSemigroupLaws,
  plutusSemigroupLawsWith,
  plutusMonoidLaws,
  plutusMonoidLawsWith,

  -- *** Numeric
  additiveSemigroupLaws,
  additiveSemigroupLawsWith,
  additiveMonoidLaws,
  additiveMonoidLawsWith,
  additiveGroupLaws,
  additiveGroupLawsWith,
  multiplicativeSemigroupLaws,
  multiplicativeMonoidLaws,
  semiringConsistencyLaws,
  semiringConsistencyLawsWith,
) where

import Data.Aeson (FromJSON, ToJSON (toJSON), decode, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as Lazy
import Data.Kind (Type)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
  fromData,
  toData,
 )
import PlutusTx.Prelude qualified as PlutusTx
import Test.QuickCheck (
  Property,
  checkCoverage,
  cover,
  forAllShrinkShow,
  property,
  (.||.),
  (===),
 )
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary,
  liftArbitrary2,
  liftShrink2,
 )
import Test.QuickCheck.Function (Fun, Function, applyFun)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Poly (A)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Arbitrary (
  Entangled (Disentangled, Entangled),
  Entangled3 (Disentangled3, Entangled3),
  PA,
  Pair (Pair),
  Triple (Triple),
 )
import Test.Tasty.QuickCheck (testProperties)
import Text.PrettyPrint (
  Doc,
  Style (lineLength),
  renderStyle,
  style,
  text,
  ($+$),
 )
import Text.Show.Pretty (ppDoc, ppShow)
import Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)

{- | Checks that 'ToJSON' and 'FromJSON' for @a@ form a partial isomorphism.

 @since 1.0
-}
jsonLaws ::
  forall (a :: Type).
  (Typeable a, ToJSON a, FromJSON a, Arbitrary a, Eq a, Show a) =>
  TestTree
jsonLaws = jsonLawsWith @a arbitrary shrink

{- | As 'jsonLaws', but with an explicit generator and shrinker.

 @since 1.0
-}
jsonLawsWith ::
  forall (a :: Type).
  (Typeable a, ToJSON a, FromJSON a, Eq a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
jsonLawsWith gen shr =
  testProperties
    ("JSON laws for " <> typeName @a)
    [ ("decode . encode = Just", forAllShrinkShow gen shr showJSON prop1)
    , ("decode . encode = Just . toJSON", forAllShrinkShow gen shr showJSON prop2)
    ]
  where
    prop1 :: a -> Property
    prop1 x = (decode . encode $ x) === Just x
    prop2 :: a -> Property
    prop2 x = (decode . encode $ x) === (Just . toJSON $ x)

{- | Checks that 'ToData' and 'FromData', as well as 'ToData' and
 'UnsafeFromData', for @a@ form a partial isomorphism.

 @since 1.0
-}
dataLaws ::
  forall (a :: Type).
  (Typeable a, ToData a, UnsafeFromData a, FromData a, Arbitrary a, Eq a, Show a) =>
  TestTree
dataLaws = dataLawsWith @a arbitrary shrink

{- | As 'dataLaws', but with an explicit generator and shrinker.

 @since 1.0
-}
dataLawsWith ::
  forall (a :: Type).
  (Typeable a, ToData a, FromData a, UnsafeFromData a, Eq a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
dataLawsWith gen shr =
  testProperties
    ("Data laws for " <> typeName @a)
    [
      ( "fromBuiltinData . toBuiltinData = Just"
      , forAllShrinkShow gen shr ppShow (go fromBuiltinData toBuiltinData)
      )
    ,
      ( "fromData . toData = Just"
      , forAllShrinkShow gen shr ppShow (go fromData toData)
      )
    ,
      ( "unsafeFromBuiltinData . toBuiltinData = id"
      , forAllShrinkShow gen shr ppShow go2
      )
    ]
  where
    go ::
      forall (b :: Type).
      (b -> Maybe a) ->
      (a -> b) ->
      a ->
      Property
    go from to x = (from . to $ x) === Just x
    go2 :: a -> Property
    go2 x = (unsafeFromBuiltinData . toBuiltinData $ x) === x

{- | Checks that the 'PlutusTx.Eq' instance for @a@ is an equivalence relation.

 = Note

 This uses a technique to avoid coverage issues arising from a low likelihood
 of independently generating identical values. This mostly affects those types
 which have a large, or infinite, number of inhabitants: if your type is
 finite and small, this will actually have the /opposite/ effect, as it would
 skew the balance of comparisons the /other/ way.

 To assist with this, coverage checking is built in: if you are seeing errors
 due to coverage, try using 'plutusEqLawsDirect' instead.

 @since 1.0
-}
plutusEqLaws ::
  forall (a :: Type).
  (Typeable a, PlutusTx.Eq a, Arbitrary a, Show a) =>
  TestTree
plutusEqLaws = plutusEqLawsWith @a arbitrary shrink

{- | As 'plutusEqLaws', but with an explicit generator and shrinker.

 = Note

 As this function (like 'plutusEqLaws') uses a technique to avoid coverage
 issues, if your generator and shrinker are restricted (especially to a
 small, finite subset of the whole type), you may get test failures due to
 poor coverage. If this happens, either use 'plutusEqLawsDirectWith', or
 change the generator and shrinker to be less constrained.

 @since 1.0
-}
plutusEqLawsWith ::
  forall (a :: Type).
  (Typeable a, PlutusTx.Eq a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
plutusEqLawsWith gen shr =
  testProperties
    ("Plutus Eq laws for " <> typeName @a)
    [
      ( "x == x"
      , forAllShrinkShow gen shr ppShow propRefl
      )
    ,
      ( "if x == y, then y == x"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propSymm
      )
    ,
      ( "if x == y and y == z, then x == z"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propTrans
      )
    ]
  where
    propRefl :: a -> Property
    propRefl x = (x PlutusTx.== x) === True
    propSymm :: Entangled a -> Property
    propSymm ent = checkCoverage
      . cover 50.0 (knownEntangled ent) "precondition known satisfied"
      $ case ent of
        Entangled x y -> (x PlutusTx.== y) === (y PlutusTx.== x)
        Disentangled x y -> (x PlutusTx.== y) === (y PlutusTx.== x)
    propTrans :: Entangled3 a -> Property
    propTrans ent3 = checkCoverage
      . cover 50.0 (knownEntangled3 ent3) "precondition known satisfied"
      $ case ent3 of
        Entangled3 x y z ->
          ((x PlutusTx.== y) && (y PlutusTx.== z)) === (x PlutusTx.== z)
        Disentangled3 x y z ->
          ((x PlutusTx./= y) || (y PlutusTx./= z))
            .||. (True === (x PlutusTx.== z))

{- | Checks that the 'PlutusTx.Eq' instance for @a@ is an equivalence relation.

 = Note

 This function tests the equivalence relation properties (specifically,
 symmetry and transitivity) directly, without relying on the
 coverage-improving technique used in 'plutusEqLaws'. This works if @a@ as a
 type is both finite and has small cardinality, as in that situation, the
 distribution of successful and unsuccessful preconditions will roughly match.
 However, if @a@ is infinite or large, this will cause a skewed case
 distribution, which will produce misleading results.

 To assist with this, coverage checking is built in to the properties this
 checks; if you find that your coverage is too low, use 'plutusEqLaws'
 instead.

 @since 1.0
-}
plutusEqLawsDirect ::
  forall (a :: Type).
  (Typeable a, PlutusTx.Eq a, Show a, Arbitrary a) =>
  TestTree
plutusEqLawsDirect = plutusEqLawsDirectWith @a arbitrary shrink

{- | As 'plutusEqLawsDirect', but with explicit generator and shrinker.

 = Note

 As this function (like 'plutusEqLawsDirect') tests equivalence relation
 properties directly, either @a@ must be both finite and small, or the
 generator and shrinker passed to this function must produce only a small and
 finite subset of the type. See the caveats on the use of 'plutusEqLawsDirect'
 as to why.

 To assist with this, coverage checking is built in to the properties this
 checks; if you find that your coverage is too low, use 'plutusEqLaws'
 instead.

 @since 1.0
-}
plutusEqLawsDirectWith ::
  forall (a :: Type).
  (Typeable a, PlutusTx.Eq a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
plutusEqLawsDirectWith gen shr =
  testProperties
    ("Plutus Eq laws for " <> typeName @a)
    [
      ( "x == x"
      , forAllShrinkShow gen shr ppShow propRefl
      )
    ,
      ( "if x == y, then y == x"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propSymm
      )
    ,
      ( "if x == y and y == z, then x == z"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propTrans
      )
    ]
  where
    propRefl :: a -> Property
    propRefl x = (x PlutusTx.== x) === True
    propSymm :: Pair a -> Property
    propSymm (Pair x y) =
      cover 50.0 (x PlutusTx.== y) "precondition satisfied" $
        (x PlutusTx.== y) === (y PlutusTx.== x)
    propTrans :: Triple a -> Property
    propTrans (Triple x y z) =
      cover
        50.0
        ((x PlutusTx.== y) && (y PlutusTx.== z))
        "precondition satisfied"
        $ ((x PlutusTx.== y) && (y PlutusTx.== z)) === (x PlutusTx.== z)

{- | Checks that the substitution property holds for @a@'s 'PlutusTx.Eq'
 instance; specifically, that for any pure function @f@, if @x == y@, then @f
 x == f y@.

 = Note

 This uses a technique to avoid coverage issues arising from a low likelihood
 of independently generating identical values. This mostly affects those types
 which have a large, or infinite, number of inhabitants: if your type is
 finite and small, this will actually have the /opposite/ effect, as it would
 skew the balance of comparisons the /other/ way.

 To assist with this, coverage checking is built in: if you are seeing errors
 due to coverage, try using 'plutusEqLawsSubstitutionDirect' instead.

 @since 2.1
-}
plutusEqLawsSubstitution ::
  forall (a :: Type).
  (Typeable a, Arbitrary a, Show a, PlutusTx.Eq a, Function a, CoArbitrary a) =>
  TestTree
plutusEqLawsSubstitution = plutusEqLawsSubstitutionWith @a arbitrary shrink

{- | As 'plutusEqLawsSubstitution', but with explicit generator and shrinker.

 = Note

 As this function (like 'plutusEqLawsSubstitution') uses a technique to avoid
 coverage issues, if your generator and shrinker are restricted (especially to
 a small, finite subset of the whole type), you may get test failures due to
 poor coverage. If this happens, either use
 'plutusEqLawsSubstitutionDirectWith', or change the generator and shrinker to
 be less constrained.

 @since 2.1
-}
plutusEqLawsSubstitutionWith ::
  forall (a :: Type).
  (Typeable a, Show a, PlutusTx.Eq a, Function a, CoArbitrary a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
plutusEqLawsSubstitutionWith gen shr =
  testProperties
    ("Plutus Eq substitutability for " <> typeName @a)
    [
      ( "if x == y then f x == f y"
      , forAllShrinkShow genWithFun shrinkWithFun ppShow propSub
      )
    ]
  where
    genWithFun :: Gen (Entangled a, Fun a PA)
    genWithFun = liftArbitrary2 (liftArbitrary gen) arbitrary
    shrinkWithFun ::
      (Entangled a, Fun a PA) ->
      [(Entangled a, Fun a PA)]
    shrinkWithFun (ent, fn) =
      liftShrink2 (liftShrink shr) shrink (ent, fn)
    propSub :: (Entangled a, Fun a PA) -> Property
    propSub (ent, fn) = checkCoverage
      . cover 50.0 (knownEntangled ent) "precondition known satisfied"
      $ case ent of
        Entangled x y -> applyFun fn x === applyFun fn y
        Disentangled x y ->
          if x PlutusTx.== y
            then (applyFun fn x PlutusTx.== applyFun fn y) === True
            else property True

{- | Checks that the substitution property holds for @a@'s 'PlutusTx.Eq'
 instance; specifically, that for any pure function @f@, if @x == y@, then @f
 x == f y@.

 = Note

 This function tests the substitution property directly, without relying on
 the coverage-improving technique used in 'plutusEqLawsSubstitution'. This
 works if @a@ as a type is both finite and has small cardinality, as in that
 situation, the distribution of successful and unsuccessful preconditions will
 roughly match. However, if @a@ is infinite or large, this will cause a skewed
 case distribution, which will produce misleading results.

 To assist with this, coverage checking is built in to the property this
 checks; if you find that your coverage is too low, use
 'plutusEqLawsSubstitution' instead.

 @since 2.1
-}
plutusEqLawsSubstitutionDirect ::
  forall (a :: Type).
  (Typeable a, Arbitrary a, Show a, PlutusTx.Eq a, Function a, CoArbitrary a) =>
  TestTree
plutusEqLawsSubstitutionDirect =
  plutusEqLawsSubstitutionDirectWith @a arbitrary shrink

{- | As 'plutusEqLawsSubstitutionDirect', but with explicit generator and
 shrinker.

 = Note

 As this function (like 'plutusEqLawsSubstitutionDirect') tests the
 substutition property directly, either @a@ must be both finite and small, or
 the generator and shrinker passed to this function must produce only a small
 and finite subset of the type. See the caveats on the use of
 'plutusEqLawsSubstitutionDirect' as to why.

 To assist with this, coverage checking is built in: if you find that your
 coverage is too low, use 'plutusEqLawsSubstitution' instead.

 @since 2.1
-}
plutusEqLawsSubstitutionDirectWith ::
  forall (a :: Type).
  (Typeable a, Show a, PlutusTx.Eq a, Function a, CoArbitrary a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
plutusEqLawsSubstitutionDirectWith gen shr =
  testProperties
    ("Plutus Eq substitutability for " <> typeName @a)
    [
      ( "if x == y then f x == f y"
      , forAllShrinkShow genWithFun shrinkWithFun ppShow propSub
      )
    ]
  where
    genWithFun :: Gen (Pair a, Fun a A)
    genWithFun = liftArbitrary2 (liftArbitrary gen) arbitrary
    shrinkWithFun :: (Pair a, Fun a A) -> [(Pair a, Fun a A)]
    shrinkWithFun (p, fn) = liftShrink2 (liftShrink shr) shrink (p, fn)
    propSub :: (Pair a, Fun a A) -> Property
    propSub (Pair x y, fn) =
      cover 50.0 (x PlutusTx.== y) "precondition satisfied" $
        if x PlutusTx.== y
          then applyFun fn x === applyFun fn y
          else property True

{- | Checks that the 'PlutusTx.Ord' instance for @a@ is a total order.

 = Note

 This uses a technique to avoid coverage issues arising from a low likelihood
 of independently generating identical values (specifically in the tests for
 antisymmetry). This mostly affects those types which have a large, or
 infinite, number of inhabitants: if your type is finite and small, this will
 actually have the /opposite/ effect, as it would skew the comparisons the
 /other/ way.

 To assist with this, coverage checking is built in: if you are seeing errors
 due to coverage, try using 'plutusOrdLawsDirect' instead.

 @since 1.0
-}
plutusOrdLaws ::
  forall (a :: Type).
  (Typeable a, PlutusTx.Ord a, Arbitrary a, Show a) =>
  TestTree
plutusOrdLaws = plutusOrdLawsWith @a arbitrary shrink

{- | As 'plutusOrdLaws', but with an explicit generator and shrinker.

 = Note

 As this function (like 'plutusOrdLaws') uses a technique to avoid coverage
 issues, if your generator and shrinker are restricted (especially to a small,
 finite subset of the whole type), you may get test failures due to poor
 coverage. If this happens, either use 'plutusOrdLawsDirectWith', or change
 the generator and shrinker to be less constrained.

 @since 1.0
-}
plutusOrdLawsWith ::
  forall (a :: Type).
  (Typeable a, PlutusTx.Ord a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
plutusOrdLawsWith gen shr =
  testProperties
    ("Plutus Ord laws for " <> typeName @a)
    [
      ( "x <= y or y <= x"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propTotal
      )
    ,
      ( "if x <= y and y <= x, then x == y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propAntiSymm
      )
    ,
      ( "if x <= y and y <= z, then x <= z"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propTrans
      )
    ]
  where
    propTotal :: Pair a -> Property
    propTotal (Pair x y) =
      ((x PlutusTx.<= y) PlutusTx.== True)
        .||. ((y PlutusTx.<= x) PlutusTx.== True)
    propAntiSymm :: Entangled a -> Property
    propAntiSymm ent = checkCoverage
      . cover 50.0 (knownEntangled ent) "precondition known satisfied"
      $ case ent of
        Entangled x y ->
          ((x PlutusTx.<= y) && (y PlutusTx.<= x)) === (x PlutusTx.== y)
        Disentangled x y ->
          ((x PlutusTx.<= y) && (y PlutusTx.<= x)) === (x PlutusTx.== y)
    propTrans :: Triple a -> Property
    propTrans (Triple x y z) = checkCoverage
      . cover 33.3 (go x y z) "x-to-y and y-to-z implies x-to-z"
      $ case (x PlutusTx.<= y, y PlutusTx.<= z) of
        (True, True) -> (x PlutusTx.<= z) === True
        (False, False) -> (x PlutusTx.<= z) === False
        _ -> property True -- any outcome is acceptable
    go :: a -> a -> a -> Bool
    go x y z = (x PlutusTx.<= y) == (y PlutusTx.<= z)

{- | Checks that the 'PlutusTx.Ord' instance for @a@ is a total order.

 = Note

 This function tests the total order properties (specifically anti-symmetry
 and transitivity) directly, without relying on the coverage-improving
 technique used in 'plutusOrdLaws'. This works if @a@ as a type is both finite
 and has small cardinality, as in that situation, the distribution of
 successful and unsuccessful preconditions will be reasonable. However, if @a@
 is infinite or large, this will cause a skewed case distribution, which will
 produce misleading results.

 To assist with this, coverage checking is built in to the properties this
 checks; if you find that your coverage is too low, use 'plutusOrdLaws'
 instead.

 @since 1.0
-}
plutusOrdLawsDirect ::
  forall (a :: Type).
  (Typeable a, PlutusTx.Ord a, Show a, Arbitrary a) =>
  TestTree
plutusOrdLawsDirect = plutusOrdLawsDirectWith @a arbitrary shrink

{- | As 'plutusOrdLawsDirect', but with explicit generator and shrinker.

 = Note

 As this function (like 'plutusOrdLawsDirect') tests total order properties
 directly, either @a@ must be both finite and small, or the generator and
 shrinker passed to this function must produce only a small and finite subset
 of the type. See the caveats on the use of 'plutusOrdLawsDirect' as to why.

 To assist with this, coverage checking is built in to the properties this
 checks; if you find that your coverage is too low, use 'plutusOrdLaws'
 instead.

 @since 1.0
-}
plutusOrdLawsDirectWith ::
  forall (a :: Type).
  (Typeable a, PlutusTx.Ord a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
plutusOrdLawsDirectWith gen shr =
  testProperties
    ("Plutus Ord laws for " <> typeName @a)
    [
      ( "x <= y or y <= x"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propTotal
      )
    ,
      ( "if x <= y and y <= x, then x == y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propAntiSymm
      )
    ,
      ( "if x <= y and y <= z, then x <= z"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propTrans
      )
    ]
  where
    propTotal :: Pair a -> Property
    propTotal (Pair x y) =
      ((x PlutusTx.<= y) PlutusTx.== True)
        .||. ((y PlutusTx.<= x) PlutusTx.== True)
    propAntiSymm :: Pair a -> Property
    propAntiSymm (Pair x y) =
      cover 50.0 (go x y) "precondition known satisfied" $
        ((x PlutusTx.<= y) && (y PlutusTx.<= x)) === (x PlutusTx.== y)
    propTrans :: Triple a -> Property
    propTrans (Triple x y z) = cover 33.3 (go2 x y z) "precondition known satisfied" $
      case (x PlutusTx.<= y, y PlutusTx.<= z) of
        (True, True) -> (x PlutusTx.<= z) === True
        (False, False) -> (x PlutusTx.<= z) === False
        _ -> property True -- any outcome is acceptable
    go :: a -> a -> Bool
    go x y = (x PlutusTx.<= y) && (y PlutusTx.<= x)
    go2 :: a -> a -> a -> Bool
    go2 x y z = (x PlutusTx.<= y) == (y PlutusTx.<= z)

{- | Checks that the 'PlutusTx.Semigroup' instance for @a@ has an associative
 'PlutusTx.<>'.

 @since 1.0
-}
plutusSemigroupLaws ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.Semigroup a, Arbitrary a, Show a) =>
  TestTree
plutusSemigroupLaws = plutusSemigroupLawsWith @a arbitrary shrink

{- | As 'plutusSemigroupLaws', but with explicit generator and shrinker.

 @since 1.0
-}
plutusSemigroupLawsWith ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.Semigroup a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
plutusSemigroupLawsWith gen shr =
  testProperties
    ("PlutusTx Semigroup laws for " <> typeName @a)
    [
      ( "(x <> y) <> z = x <> (y <> z)"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow go
      )
    ]
  where
    go :: Triple a -> Property
    go (Triple x y z) =
      ((x PlutusTx.<> y) PlutusTx.<> z)
        === (x PlutusTx.<> (y PlutusTx.<> z))

{- | Checks that the 'PlutusTx.Monoid' instance for @a@ has 'PlutusTx.mempty' as
 both a left and right identity for 'PlutusTx.<>'.

 @since 1.0
-}
plutusMonoidLaws ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.Monoid a, Arbitrary a, Show a) =>
  TestTree
plutusMonoidLaws = plutusMonoidLawsWith @a arbitrary shrink

{- | As 'plutusMonoidLaws', but with explicit generator and shrinker.

 @since 1.0
-}
plutusMonoidLawsWith ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.Monoid a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
plutusMonoidLawsWith gen shr =
  testProperties
    ("PlutusTx Monoid laws for " <> typeName @a)
    [ ("x <> mempty = x", forAllShrinkShow gen shr ppShow rightId)
    , ("mempty <> x = x", forAllShrinkShow gen shr ppShow leftId)
    ]
  where
    leftId :: a -> Property
    leftId x = x PlutusTx.<> PlutusTx.mempty === x
    rightId :: a -> Property
    rightId x = PlutusTx.mempty PlutusTx.<> x === x

{- | Checks that the 'PlutusTx.AdditiveSemigroup' instance for @a@ is
 associative and commutative.

 @since 2.2
-}
additiveSemigroupLaws ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.AdditiveSemigroup a, Arbitrary a, Show a) =>
  TestTree
additiveSemigroupLaws = additiveSemigroupLawsWith @a arbitrary shrink

{- | As 'additiveSemigroupLaws', but with explicit generator and shrinker.

 @since 2.2
-}
additiveSemigroupLawsWith ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.AdditiveSemigroup a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
additiveSemigroupLawsWith gen shr =
  testProperties
    ("Plutus AdditiveSemigroup laws for " <> typeName @a)
    [
      ( "x + y = y + x"
      , forAllShrinkShow
          (liftArbitrary gen)
          (liftShrink shr)
          ppShow
          commLaw
      )
    ,
      ( "x + (y + z) = (x + y) + z"
      , forAllShrinkShow
          (liftArbitrary gen)
          (liftShrink shr)
          ppShow
          assocLaw
      )
    ]
  where
    commLaw :: Pair a -> Property
    commLaw (Pair x y) = x PlutusTx.+ y === y PlutusTx.+ x
    assocLaw :: Triple a -> Property
    assocLaw (Triple x y z) =
      (x PlutusTx.+ (y PlutusTx.+ z)) === ((x PlutusTx.+ y) PlutusTx.+ z)

{- | Checks that 'PlutusTx.zero' for @a@ is a left and right identity for
 'PlutusTx.+'.

 @since 2.2
-}
additiveMonoidLaws ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.AdditiveMonoid a, Arbitrary a, Show a) =>
  TestTree
additiveMonoidLaws = additiveMonoidLawsWith @a arbitrary shrink

{- | As 'additiveMonoidLaws' but with explicit generator and shrinker.

 @since 2.2
-}
additiveMonoidLawsWith ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.AdditiveMonoid a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
additiveMonoidLawsWith gen shr =
  testProperties
    ("Plutus AdditiveMonoid laws for " <> typeName @a)
    [ ("x + 0 = x", forAllShrinkShow gen shr ppShow rightId)
    , ("0 + x = x", forAllShrinkShow gen shr ppShow leftId)
    ]
  where
    leftId :: a -> Property
    leftId x = x PlutusTx.+ PlutusTx.zero === x
    rightId :: a -> Property
    rightId x = PlutusTx.zero PlutusTx.+ x === x

{- | Checks that 'PlutusTx.-' is an inverse operation to 'PlutusTx.+'.

 @since 2.2
-}
additiveGroupLaws ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.AdditiveGroup a, Arbitrary a, Show a) =>
  TestTree
additiveGroupLaws = additiveGroupLawsWith @a arbitrary shrink

{- | As 'additiveGroupLaws', but with explicit generator and shrinker.

 @since 2.2
-}
additiveGroupLawsWith ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.AdditiveGroup a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
additiveGroupLawsWith gen shr =
  testProperties
    ("Plutus AdditiveGroup laws for " <> typeName @a)
    [
      ( "x + y - y = x"
      , forAllShrinkShow
          (liftArbitrary gen)
          (liftShrink shr)
          ppShow
          invLaw
      )
    ]
  where
    invLaw :: Pair a -> Property
    invLaw (Pair x y) = x PlutusTx.+ y PlutusTx.- y === x

{- | Checks that 'PlutusTx.*' is associative. Since multiplication does not have
 to be commutative, we don't check this law.

 @since 2.2
-}
multiplicativeSemigroupLaws ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.MultiplicativeSemigroup a, Arbitrary a, Show a) =>
  TestTree
multiplicativeSemigroupLaws = multiplicativeSemigroupLawsWith @a arbitrary shrink

{- | As 'multiplicativeSemigroupLaws', but with explicit generator and shrinker.

 @since 2.2
-}
multiplicativeSemigroupLawsWith ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.MultiplicativeSemigroup a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
multiplicativeSemigroupLawsWith gen shr =
  testProperties
    ("Plutus MultiplicativeSemigroup laws for " <> typeName @a)
    [
      ( "x * (y * z) = (x * y) * z"
      , forAllShrinkShow
          (liftArbitrary gen)
          (liftShrink shr)
          ppShow
          assocLaw
      )
    ]
  where
    assocLaw :: Triple a -> Property
    assocLaw (Triple x y z) =
      (x PlutusTx.* (y PlutusTx.* z)) === ((x PlutusTx.* y) PlutusTx.* z)

{- | Checks that 'PlutusTx.one' for @a@ is a left and right identity for
 'PlutusTx.*'.

 @since 2.2
-}
multiplicativeMonoidLaws ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.MultiplicativeMonoid a, Arbitrary a, Show a) =>
  TestTree
multiplicativeMonoidLaws = multiplicativeMonoidLawsWith @a arbitrary shrink

{- | As 'multiplicativeMonoidLaws' but with explicit generator and shrinker.

 @since 2.2
-}
multiplicativeMonoidLawsWith ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.MultiplicativeMonoid a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
multiplicativeMonoidLawsWith gen shr =
  testProperties
    ("Plutus MultiplicativeMonoid laws for " <> typeName @a)
    [ ("x * 1 = x", forAllShrinkShow gen shr ppShow rightId)
    , ("1 * x = x", forAllShrinkShow gen shr ppShow leftId)
    ]
  where
    leftId :: a -> Property
    leftId x = x PlutusTx.* PlutusTx.one === x
    rightId :: a -> Property
    rightId x = PlutusTx.one PlutusTx.* x === x

{- | Checks that 'PlutusTx.zero' is an annihilating element for 'PlutusTx.*',
 and that 'PlutusTx.*' distributes left and right over 'PlutusTx.+'.

 = Note

 This does not check the requirements for 'PlutusTx.+' and 'PlutusTx.*'
 individually as operations of a semiring: use the other law checks to do
 this. The name of this function reflects this, as the goal is to check that
 the two inter-operate correctly.

 @since 2.2
-}
semiringConsistencyLaws ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.Semiring a, Arbitrary a, Show a) =>
  TestTree
semiringConsistencyLaws = semiringConsistencyLawsWith @a arbitrary shrink

{- | As 'semiringConsistencyLaws', but with explicit generator and shrinker.

 @since 2.2
-}
semiringConsistencyLawsWith ::
  forall (a :: Type).
  (Typeable a, Eq a, PlutusTx.Semiring a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
semiringConsistencyLawsWith gen shr =
  testProperties
    ("Plutus Semiring laws for " <> typeName @a)
    [ ("0 * x = 0", forAllShrinkShow gen shr ppShow leftAnnihilation)
    , ("x * 0 = 0", forAllShrinkShow gen shr ppShow rightAnnihilation)
    ,
      ( "x * (y + z) = (x * y) + (x * z)"
      , forAllShrinkShow
          (liftArbitrary gen)
          (liftShrink shr)
          ppShow
          leftDistr
      )
    ,
      ( "(x + y) * z = (x * z) + (y * z)"
      , forAllShrinkShow
          (liftArbitrary gen)
          (liftShrink shr)
          ppShow
          rightDistr
      )
    ]
  where
    leftAnnihilation :: a -> Property
    leftAnnihilation x = PlutusTx.zero PlutusTx.* x === PlutusTx.zero
    rightAnnihilation :: a -> Property
    rightAnnihilation x = x PlutusTx.* PlutusTx.zero === PlutusTx.zero
    leftDistr :: Triple a -> Property
    leftDistr (Triple x y z) =
      x PlutusTx.* (y PlutusTx.+ z) === (x PlutusTx.* y) PlutusTx.+ (x PlutusTx.* z)
    rightDistr :: Triple a -> Property
    rightDistr (Triple x y z) =
      (x PlutusTx.+ y) PlutusTx.* z === (x PlutusTx.* z) PlutusTx.+ (y PlutusTx.* z)

-- Helpers

knownEntangled ::
  forall (a :: Type).
  Entangled a ->
  Bool
knownEntangled = \case
  Entangled _ _ -> True
  _ -> False

knownEntangled3 ::
  forall (a :: Type).
  Entangled3 a ->
  Bool
knownEntangled3 = \case
  Entangled3 {} -> True
  _ -> False

typeName :: forall (a :: Type). (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a

showJSON ::
  forall (a :: Type).
  (Show a, ToJSON a) =>
  a ->
  String
showJSON x =
  renderStyle ourStyle $
    "As data type"
      $+$ ""
      $+$ ppDoc x
      $+$ ""
      $+$ "As JSON"
      $+$ go
  where
    go :: Doc
    go = text . T.unpack . decodeUtf8 . Lazy.toStrict . encodePretty $ x

ourStyle :: Style
ourStyle = style {lineLength = 80}

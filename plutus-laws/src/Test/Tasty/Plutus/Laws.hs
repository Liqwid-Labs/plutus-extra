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

  -- *** Ord
  plutusOrdLaws,
  plutusOrdLawsWith,
  plutusOrdLawsDirect,
  plutusOrdLawsDirectWith,

  -- *** Others
  plutusSemigroupLaws,
  plutusSemigroupLawsWith,
  plutusMonoidLaws,
  plutusMonoidLawsWith,
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
  elements,
  forAllShrinkShow,
  (.||.),
  (===),
  (==>),
 )
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
 )
import Test.QuickCheck.Gen (Gen)
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Arbitrary (
  Entangled (Disentangled, Entangled),
  Entangled3 (Disentangled3, Entangled3),
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
 finite and small, this will actually have the _opposite_ effect, as it would
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

{- | Checks that the 'PlutusTx.Ord' instance for @a@ is a total order.

 = Note

 This uses a technique to avoid coverage issues arising from a low likelihood
 of independently generating identical values (specifically in the tests for
 antisymmetry). This mostly affects those types which have a large, or
 infinite, number of inhabitants: if your type is finite and small, this will
 actually have the _opposite_ effect, as it would skew the comparisons the
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
      ( "x <= x"
      , forAllShrinkShow gen shr ppShow propRefl
      )
    ,
      ( "if x <= y and y <= z, then x <= z"
      , forAllShrinkShow sortedTripleGen (liftShrink shr) ppShow propTrans
      )
    ,
      ( "x >= y if and only if y <= x"
      , forAllShrinkShow pairGen (liftShrink shr) ppShow propGteLte
      )
    ,
      ( "x < y if and only if x <= y and x /= y"
      , forAllShrinkShow pairGen (liftShrink shr) ppShow propLtLte
      )
    ,
      ( "x > y if and only if y < x"
      , forAllShrinkShow pairGen (liftShrink shr) ppShow propGtLt
      )
    ,
      ( "compare x y == LT if and only if x < y"
      , forAllShrinkShow pairGen (liftShrink shr) ppShow propCompareLt
      )
    ,
      ( "compare x y == GT if and only if x > y"
      , forAllShrinkShow pairGen (liftShrink shr) ppShow propCompareGt
      )
    ,
      ( "compare x y == EQ if and only if x == y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propCompareEq
      )
    ,
      ( "min x y == if x <= y then x else y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propMin
      )
    ,
      ( "max x y == if x >= y then x else y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propMax
      )
    ]
  where
    propTotal :: Pair a -> Property
    propTotal (Pair x y) = (x PlutusTx.<= y) .||. (y PlutusTx.<= x)
    propAntiSymm :: Entangled a -> Property
    propAntiSymm ent =
      checkCoverage
        . cover 50.0 (knownEntangled ent) "precondition known satisfied"
        $ case ent of
          Entangled x y ->
            ((x PlutusTx.<= y) && (y PlutusTx.<= x)) === (x PlutusTx.== y)
          Disentangled x y ->
            ((x PlutusTx.<= y) && (y PlutusTx.<= x)) === (x PlutusTx.== y)
    propRefl :: a -> Property
    propRefl x = (x PlutusTx.<= x) === True
    propTrans :: Triple a -> Property
    propTrans (Triple x y z) =
      x PlutusTx.<= y && y PlutusTx.<= z ==> x PlutusTx.<= z
    propGteLte :: Pair a -> Property
    propGteLte (Pair x y) =
      checkCoverage
        . cover 50.0 (y PlutusTx.<= x) "precondition (y <= x) satisfied"
        $ (x PlutusTx.>= y) === (y PlutusTx.<= x)
    propLtLte :: Pair a -> Property
    propLtLte (Pair x y) =
      checkCoverage
        . cover 40.0 (go x y) "precondition (x <= y and x /= y) satisfied"
        $ (x PlutusTx.< y) === (x PlutusTx.<= y && x PlutusTx./= y)
    propGtLt :: Pair a -> Property
    propGtLt (Pair x y) =
      checkCoverage
        . cover 40.0 (y PlutusTx.< x) "precondition (y < x) satisfied"
        $ (x PlutusTx.> y) === (y PlutusTx.< x)
    propCompareLt :: Pair a -> Property
    propCompareLt (Pair x y) =
      checkCoverage
        . cover 40.0 (x PlutusTx.< y) "precondition (x < y) satisfied"
        $ (PlutusTx.compare x y == LT) === (x PlutusTx.< y)
    propCompareGt :: Pair a -> Property
    propCompareGt (Pair x y) =
      checkCoverage
        . cover 40.0 (x PlutusTx.> y) "precondition (x > y) satisfied"
        $ (PlutusTx.compare x y == GT) === (x PlutusTx.> y)
    propCompareEq :: Entangled a -> Property
    propCompareEq ent =
      checkCoverage
        . cover 50.0 (knownEntangled ent) "precondition known satisfied"
        $ case ent of
          Entangled x y ->
            PlutusTx.compare x y === EQ
          Disentangled x y ->
            (PlutusTx.compare x y == EQ) === (x PlutusTx.== y)
    propMin :: Pair a -> Property
    propMin (Pair x y) =
      checkCoverage
        . cover 50.0 (x PlutusTx.<= y) "precondition (x <= y) satisfied"
        $ PlutusTx.min x y PlutusTx.== if x PlutusTx.<= y then x else y
    propMax :: Pair a -> Property
    propMax (Pair x y) =
      checkCoverage
        . cover 50.0 (x PlutusTx.>= y) "precondition (x >= y) satisfied"
        $ PlutusTx.max x y PlutusTx.== if x PlutusTx.>= y then x else y
    go :: a -> a -> Bool
    go x y = x PlutusTx.<= y && x PlutusTx./= y
    sortedTripleGen :: Gen (Triple a)
    sortedTripleGen = do
      Pair a b <- liftArbitrary gen
      c <- gen
      sortTriple
        <$> elements
          [ Triple a a a
          , Triple a a b
          , Triple a b c
          ]
    pairGen :: Gen (Pair a)
    pairGen = do
      Pair a b <- liftArbitrary gen
      let le = PlutusTx.min a b
          gt = PlutusTx.max a b
      elements [Pair le gt, Pair gt le]

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
      ( "x <= x"
      , forAllShrinkShow gen shr ppShow propRefl
      )
    ,
      ( "if x <= y and y <= z, then x <= z"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propTrans
      )
    ,
      ( "x >= y if and only if y <= x"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propGteLte
      )
    ,
      ( "x < y if and only if x <= y and x /= y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propLtLte
      )
    ,
      ( "x > y if and only if y < x"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propGtLt
      )
    ,
      ( "compare x y == LT if and only if x < y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propCompareLt
      )
    ,
      ( "compare x y == GT if and only if x > y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propCompareGt
      )
    ,
      ( "compare x y == EQ if and only if x == y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propCompareEq
      )
    ,
      ( "min x y == if x <= y then x else y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propMin
      )
    ,
      ( "max x y == if x >= y then x else y"
      , forAllShrinkShow (liftArbitrary gen) (liftShrink shr) ppShow propMax
      )
    ]
  where
    propTotal :: Pair a -> Property
    propTotal (Pair x y) = (x PlutusTx.<= y) .||. (y PlutusTx.<= x)
    propAntiSymm :: Pair a -> Property
    propAntiSymm (Pair x y) =
      cover 50.0 (go x y) "precondition (x <= y and y <= x) satisfied" $
        ((x PlutusTx.<= y) && (y PlutusTx.<= x)) === (x PlutusTx.== y)
    propRefl :: a -> Property
    propRefl x = (x PlutusTx.<= x) === True
    propTrans :: Triple a -> Property
    propTrans triple =
      let Triple x y z = sortTriple triple
       in x PlutusTx.<= y && y PlutusTx.<= z ==> x PlutusTx.<= z
    propGteLte :: Pair a -> Property
    propGteLte (Pair x y) =
      cover 50.0 (y PlutusTx.<= x) "precondition (y <= x) satisfied" $
        (x PlutusTx.>= y) === (y PlutusTx.<= x)
    propLtLte :: Pair a -> Property
    propLtLte (Pair x y) =
      cover 40.0 (go2 x y) "precondition (x <= y and x /= y) satisfied" $
        (x PlutusTx.< y) === (x PlutusTx.<= y && x PlutusTx./= y)
    propGtLt :: Pair a -> Property
    propGtLt (Pair x y) =
      cover 40.0 (y PlutusTx.< x) "precondition (y < x) satisfied" $
        (x PlutusTx.> y) === (y PlutusTx.< x)
    propCompareLt :: Pair a -> Property
    propCompareLt (Pair x y) =
      cover 40.0 (x PlutusTx.< y) "precondition (x < y) satisfied" $
        (PlutusTx.compare x y == LT) === (x PlutusTx.< y)
    propCompareGt :: Pair a -> Property
    propCompareGt (Pair x y) =
      cover 40.0 (x PlutusTx.> y) "precondition (x > y) satisfied" $
        (PlutusTx.compare x y == GT) === (x PlutusTx.> y)
    propCompareEq :: Pair a -> Property
    propCompareEq (Pair x y) =
      cover 50.0 (x PlutusTx.== y) "precondition (x = y) satisfied" $
        (PlutusTx.compare x y == EQ) === (x PlutusTx.== y)
    propMin :: Pair a -> Property
    propMin (Pair x y) =
      cover 50.0 (x PlutusTx.<= y) "precondition (x <= y) satisfied" $
        PlutusTx.min x y PlutusTx.== if x PlutusTx.<= y then x else y
    propMax :: Pair a -> Property
    propMax (Pair x y) =
      cover 50.0 (x PlutusTx.>= y) "precondition (x >= y) satisfied" $
        PlutusTx.max x y PlutusTx.== if x PlutusTx.>= y then x else y
    go :: a -> a -> Bool
    go x y = x PlutusTx.<= y && y PlutusTx.<= x
    go2 :: a -> a -> Bool
    go2 x y = x PlutusTx.<= y && x PlutusTx./= y

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

-- Helpers

sortTriple ::
  forall (a :: Type).
  (PlutusTx.Ord a) =>
  Triple a ->
  Triple a
sortTriple (Triple a b c) =
  case (PlutusTx.compare a b, PlutusTx.compare b c, PlutusTx.compare a c) of
    (GT, GT, _) -> Triple c b a
    (GT, _, GT) -> Triple b c a
    (GT, _, _) -> Triple b a c
    (_, GT, GT) -> Triple c a b
    (_, GT, _) -> Triple a c b
    (_, _, _) -> Triple a b c

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

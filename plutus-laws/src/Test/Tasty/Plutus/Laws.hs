{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Tasty.Plutus.Laws (
  -- * Test API
  jsonLaws,
  jsonLawsWith,
  dataLaws,
  dataLawsWith,
  plutusEqLaws,
  plutusEqLawsWith,
  --  plutusOrdLaws,
  --  plutusOrdLawsWith

  -- * Helper type
  Entangled (Entangled, Disentangled),
  Entangled3 (Entangled3, Disentangled3),
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
import Test.QuickCheck (Property, forAllShrinkShow, (.||.), (===))
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
 )
import Test.QuickCheck.Gen (Gen)
import Test.Tasty (TestTree)
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

-- | @since 1.0
jsonLaws ::
  forall (a :: Type).
  (Typeable a, ToJSON a, FromJSON a, Arbitrary a, Eq a, Show a) =>
  TestTree
jsonLaws = jsonLawsWith @a arbitrary shrink

-- | @since 1.0
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

-- | @since 1.0
dataLaws ::
  forall (a :: Type).
  (Typeable a, ToData a, UnsafeFromData a, FromData a, Arbitrary a, Eq a, Show a) =>
  TestTree
dataLaws = dataLawsWith @a arbitrary shrink

-- | @since 1.0
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

-- | @since 1.0
plutusEqLaws ::
  forall (a :: Type).
  (Typeable a, PlutusTx.Eq a, Arbitrary a, Show a) =>
  TestTree
plutusEqLaws = plutusEqLawsWith @a arbitrary shrink

-- | @since 1.0
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
    propSymm ent = case ent of
      Entangled x y -> (x PlutusTx.== y) === (y PlutusTx.== x)
      Disentangled x y -> (x PlutusTx.== y) === (y PlutusTx.== x)
    propTrans :: Entangled3 a -> Property
    propTrans = \case
      Entangled3 x y z ->
        ((x PlutusTx.== y) && (y PlutusTx.== z)) === (x PlutusTx.== z)
      Disentangled3 x y z ->
        ((x PlutusTx./= y) || (y PlutusTx./= z))
          .||. (True === (x PlutusTx.== z))

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
delta :: Entangled a -> Maybe (a, a)
delta = \case
  Same x -> Just (x, x)
  PossiblyDifferent _ _ -> Nothing

-- Helper for the view pattern on Entangled3
delta3 :: Entangled3 a -> Maybe (a, a, a)
delta3 = \case
  Same3 x -> Just (x, x, x)
  PossiblyDifferent3 {} -> Nothing

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

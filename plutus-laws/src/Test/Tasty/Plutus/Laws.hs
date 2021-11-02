{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Tasty.Plutus.Laws (
  jsonLaws,
  jsonLawsWith,
  dataLaws,
  dataLawsWith,
  --  unsafeDataLaws,
  --  unsafeDataLawsWith,
  --  plutusEqLaws,
  --  plutusEqLawsWith,
  --  plutusOrdLaws,
  --  plutusOrdLawsWith
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
  fromData,
  toData,
 )
import Test.QuickCheck (Property, forAllShrinkShow, (===))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
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
  (Typeable a, ToData a, FromData a, Arbitrary a, Eq a, Show a) =>
  TestTree
dataLaws = dataLawsWith @a arbitrary shrink

-- | @since 1.0
dataLawsWith ::
  forall (a :: Type).
  (Typeable a, ToData a, FromData a, Eq a, Show a) =>
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
    ]
  where
    go ::
      forall (b :: Type).
      (b -> Maybe a) ->
      (a -> b) ->
      a ->
      Property
    go from to x = (from . to $ x) === Just x

-- Helpers

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

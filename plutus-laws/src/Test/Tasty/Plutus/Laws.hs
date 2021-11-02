{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Tasty.Plutus.Laws (
  jsonLaws,
  jsonLawsWith,
--  dataLaws,
--  dataLawsWith,
--  unsafeDataLaws,
--  unsafeDataLawsWith,
--  plutusEqLaws,
--  plutusEqLawsWith,
--  plutusOrdLaws,
--  plutusOrdLawsWith
  ) where

import Data.Text.Encoding (decodeUtf8)
import Data.Text qualified as T
import Data.ByteString.Lazy qualified as Lazy
import Text.PrettyPrint (renderStyle, Style (lineLength), ($+$), 
  Doc, text, style)
import Data.Aeson.Encode.Pretty (encodePretty)
import Text.Show.Pretty (ppDoc)
import Type.Reflection (Typeable, tyConName, typeRepTyCon, typeRep)
import Test.Tasty.QuickCheck (testProperties)
import Data.Aeson (ToJSON (toJSON), FromJSON, decode, encode)
import Data.Kind (Type)
import Test.Tasty (TestTree)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck (forAllShrinkShow, Property, (===)) 

-- | @since 1.0
jsonLaws :: forall (a :: Type) . 
  (Typeable a, ToJSON a, FromJSON a, Arbitrary a, Eq a, Show a) => 
  TestTree
jsonLaws = jsonLawsWith @a arbitrary shrink

-- | @since 1.0
jsonLawsWith :: forall (a :: Type) . 
  (Typeable a, ToJSON a, FromJSON a, Eq a, Show a) => 
  Gen a -> 
  (a -> [a]) -> 
  TestTree
jsonLawsWith gen shr = testProperties ("JSON laws for " <> typeName @a) [
  ("decode . encode = Just", forAllShrinkShow gen shr showJSON prop1),
  ("decode . encode = Just . toJSON", forAllShrinkShow gen shr showJSON prop2)
  ]
  where
    prop1 :: a -> Property
    prop1 x = (decode . encode $ x) === Just x
    prop2 :: a -> Property
    prop2 x = (decode . encode $ x) === (Just . toJSON $ x)

-- Helpers

typeName :: forall (a :: Type) . (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a

showJSON :: forall (a :: Type) . 
  (Show a, ToJSON a) => a -> String
showJSON x = renderStyle ourStyle $ 
  "As data type" $+$
  "" $+$
  ppDoc x $+$
  "" $+$
  "As JSON" $+$
  go
  where
    go :: Doc
    go = text . T.unpack . decodeUtf8 . Lazy.toStrict . encodePretty $ x

ourStyle :: Style
ourStyle = style { lineLength = 80 }

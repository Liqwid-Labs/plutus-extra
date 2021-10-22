{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Test.Tasty.Plutus.Golden (
  goldenJSON,
  goldenData,
  goldenUnsafeData,
  goldenToSchema,
  goldenToArgument,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import PlutusTx.IsData.Class (FromData, ToData, UnsafeFromData)
import Schema qualified as Plutus
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Generator (Generator)
import Test.Tasty.Plutus.Golden.JSON (doGoldenJSON)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
 )
import Type.Reflection (
  Typeable,
  tyConName,
  typeRep,
  typeRepTyCon,
 )

goldenJSON ::
  forall (a :: Type).
  (Typeable a, ToJSON a, FromJSON a) =>
  Generator a ->
  TestTree
goldenJSON = singleTest ("Golden JSON: " <> typeName @a) . GoldenJSON

goldenData ::
  forall (a :: Type).
  (Typeable a, FromData a, ToData a) =>
  Generator a ->
  TestTree
goldenData = singleTest ("Golden Data: " <> typeName @a) . GoldenData

goldenUnsafeData ::
  forall (a :: Type).
  (Typeable a, UnsafeFromData a, ToData a) =>
  Generator a ->
  TestTree
goldenUnsafeData = singleTest ("Golden UnsafeData: " <> typeName @a) . GoldenUnsafeData

goldenToSchema ::
  forall (a :: Type).
  (Typeable a, Plutus.ToSchema a) =>
  Generator a ->
  TestTree
goldenToSchema = singleTest ("Golden ToSchema: " <> typeName @a) . GoldenToSchema

goldenToArgument ::
  forall (a :: Type).
  (Typeable a, Plutus.ToArgument a) =>
  Generator a ->
  TestTree
goldenToArgument = singleTest ("Golden ToArgument: " <> typeName @a) . GoldenToArgument

-- Helpers

typeName :: forall (a :: Type). (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a

data GoldenTest (a :: Type) where
  GoldenJSON :: (ToJSON a, FromJSON a) => Generator a -> GoldenTest a
  GoldenData :: (FromData a, ToData a) => Generator a -> GoldenTest a
  GoldenUnsafeData :: (UnsafeFromData a, ToData a) => Generator a -> GoldenTest a
  GoldenToSchema :: (Plutus.ToSchema a) => Generator a -> GoldenTest a
  GoldenToArgument :: (Plutus.ToArgument a) => Generator a -> GoldenTest a

instance (Typeable a) => IsTest (GoldenTest a) where
  run _ gt _ = case gt of
    GoldenJSON gen -> doGoldenJSON gen
    GoldenData _ -> pure . testFailed $ "Unsupported for now"
    GoldenUnsafeData _ -> pure . testFailed $ "Unsupported for now"
    GoldenToSchema _ -> pure . testFailed $ "Unsupported for now"
    GoldenToArgument _ -> pure . testFailed $ "Unsupported for now"
  testOptions = Tagged []

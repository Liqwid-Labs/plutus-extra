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
goldenJSON = singleTest ("Golden JSON: " <> tyName) . GoldenJSON tyName
  where
    tyName :: String
    tyName = typeName @a

goldenData ::
  forall (a :: Type).
  (Typeable a, FromData a, ToData a) =>
  Generator a ->
  TestTree
goldenData = singleTest ("Golden Data: " <> tyName) . GoldenData tyName
  where
    tyName :: String
    tyName = typeName @a

goldenUnsafeData ::
  forall (a :: Type).
  (Typeable a, UnsafeFromData a, ToData a) =>
  Generator a ->
  TestTree
goldenUnsafeData =
  singleTest ("Golden UnsafeData: " <> tyName) . GoldenUnsafeData tyName
  where
    tyName :: String
    tyName = typeName @a

goldenToSchema ::
  forall (a :: Type).
  (Typeable a, Plutus.ToSchema a) =>
  Generator a ->
  TestTree
goldenToSchema =
  singleTest ("Golden ToSchema: " <> tyName) . GoldenToSchema tyName
  where
    tyName :: String
    tyName = typeName @a

goldenToArgument ::
  forall (a :: Type).
  (Typeable a, Plutus.ToArgument a) =>
  Generator a ->
  TestTree
goldenToArgument =
  singleTest ("Golden ToArgument: " <> tyName) . GoldenToArgument tyName
  where
    tyName :: String
    tyName = typeName @a

-- Helpers

typeName :: forall (a :: Type). (Typeable a) => String
typeName = tyConName . typeRepTyCon $ typeRep @a

data GoldenTest (a :: Type) where
  GoldenJSON :: (ToJSON a, FromJSON a) => String -> Generator a -> GoldenTest a
  GoldenData :: (FromData a, ToData a) => String -> Generator a -> GoldenTest a
  GoldenUnsafeData :: (UnsafeFromData a, ToData a) => String -> Generator a -> GoldenTest a
  GoldenToSchema :: (Plutus.ToSchema a) => String -> Generator a -> GoldenTest a
  GoldenToArgument :: (Plutus.ToArgument a) => String -> Generator a -> GoldenTest a

instance (Typeable a) => IsTest (GoldenTest a) where
  run _ gt _ = case gt of
    GoldenJSON tyName gen -> doGoldenJSON tyName gen
    GoldenData _ _ -> pure . testFailed $ "Unsupported for now"
    GoldenUnsafeData _ _ -> pure . testFailed $ "Unsupported for now"
    GoldenToSchema _ _ -> pure . testFailed $ "Unsupported for now"
    GoldenToArgument _ _ -> pure . testFailed $ "Unsupported for now"
  testOptions = Tagged []

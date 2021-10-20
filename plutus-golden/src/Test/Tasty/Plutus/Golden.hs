{-# LANGUAGE RankNTypes #-}

module Test.Tasty.Plutus.Golden (
  goldenJSON,
  goldenData,
  goldenUnsafeData,
  goldenToSchema,
  goldenToArgument
  ) where

import Schema qualified as Plutus
import PlutusTx.IsData.Class (FromData, ToData, UnsafeFromData)
import System.Random.Stateful (StatefulGen)
import Test.Tasty (TestTree)
import Data.Aeson (ToJSON, FromJSON)
import Data.Kind (Type)
import Type.Reflection (Typeable)

goldenJSON :: forall (a :: Type) . 
  (Typeable a, ToJSON a, FromJSON a) => 
  (forall (g :: Type) (m :: Type -> Type) .  (StatefulGen g m) => g -> m a) ->
  TestTree
goldenJSON gen = _

goldenData :: forall (a :: Type) . 
  (Typeable a, FromData a, ToData a) => 
  (forall (g :: Type) (m :: Type -> Type) . (StatefulGen g m) => g -> m a) -> 
  TestTree
goldenData gen = _

goldenUnsafeData :: forall (a :: Type) . 
  (Typeable a, UnsafeFromData a, ToData a) => 
  (forall (g :: Type) (m :: Type -> Type) . (StatefulGen g m) => g -> m a) -> 
  TestTree
goldenUnsafeData gen = _

goldenToSchema :: forall (a :: Type) . 
  (Typeable a, Plutus.ToSchema a) => 
  (forall (g :: Type) (m :: Type -> Type) . (StatefulGen g m) => g -> m a) -> 
  TestTree
goldenToSchema gen = _

goldenToArgument :: forall (a :: Type) . 
  (Typeable a, Plutus.ToArgument a) => 
  (forall (g :: Type) (m :: Type -> Type) . (StatefulGen g m) => g -> m a) -> 
  TestTree
goldenToArgument gen = _

module Test.Tasty.Plutus.Internal.TestScript (
  TestValidator (..),
  TestMintingPolicy (..),
  
  mkTestValidator,
  mkTestValidatorUnsafe,
  
  mkTestMintingPolicy,
  mkTestMintingPolicyUnsafe,

) where

import Data.Kind (Type)
import PlutusTx.Builtins (BuiltinData)
import Plutus.V1.Ledger.Api (MintingPolicy, mkValidatorScript, mkMintingPolicyScript)
import Ledger.Typed.Scripts (Validator)
import PlutusTx (CompiledCode, applyCode)

newtype TestValidator (datum :: Type) (redeemer :: Type) =
  TestValidator {getTestValidator :: Validator}
  deriving stock (Show)

mkTestValidator ::
  forall (datum :: Type) (redeemer :: Type) (ctx :: Type).
  CompiledCode (datum -> redeemer -> ctx -> Bool) ->
  CompiledCode ((datum -> redeemer -> ctx -> Bool) -> (BuiltinData -> BuiltinData -> BuiltinData -> ())) ->
  TestValidator datum redeemer
mkTestValidator v wr = TestValidator $ mkValidatorScript $ wr `applyCode` v

mkTestValidatorUnsafe ::
  forall (d :: Type) (r :: Type) (datum :: Type) (redeemer :: Type) (ctx :: Type).
  CompiledCode (datum -> redeemer -> ctx -> Bool) ->
  CompiledCode ((datum -> redeemer -> ctx -> Bool) -> (BuiltinData -> BuiltinData -> BuiltinData -> ())) ->
  TestValidator d r
mkTestValidatorUnsafe v wr = TestValidator $ mkValidatorScript $ wr `applyCode` v

newtype TestMintingPolicy (redeemer :: Type) =
  TestMintingPolicy {getTestMintingPolicy :: MintingPolicy}
  deriving stock (Show)

mkTestMintingPolicy ::
  forall (redeemer :: Type) (ctx :: Type).
  CompiledCode (redeemer -> ctx -> Bool) ->
  CompiledCode ((redeemer -> ctx -> Bool) -> (BuiltinData -> BuiltinData -> ())) ->
  TestMintingPolicy redeemer
mkTestMintingPolicy mp wr = TestMintingPolicy $ mkMintingPolicyScript $ wr `applyCode` mp

mkTestMintingPolicyUnsafe ::
  forall (r :: Type)(redeemer :: Type) (ctx :: Type).
  CompiledCode (redeemer -> ctx -> Bool) ->
  CompiledCode ((redeemer -> ctx -> Bool) -> (BuiltinData -> BuiltinData -> ())) ->
  TestMintingPolicy r
mkTestMintingPolicyUnsafe mp wr = TestMintingPolicy $ mkMintingPolicyScript $ wr `applyCode` mp
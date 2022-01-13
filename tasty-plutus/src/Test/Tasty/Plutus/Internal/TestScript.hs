{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Plutus.Internal.TestScript (
  TestValidator (..),
  TestMintingPolicy (..),
  WrappedValidator (..),
  WrappedMintingPolicy (..),
  mkTestValidator,
  mkTestValidatorUnsafe,
  mkTestMintingPolicy,
  mkTestMintingPolicyUnsafe,
) where

import Data.Kind (Type)
import Ledger.Typed.Scripts (Validator)
import Plutus.V1.Ledger.Api (MintingPolicy, mkMintingPolicyScript, mkValidatorScript)
import PlutusTx (CompiledCode, applyCode)
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.TH (compile)

newtype TestValidator (datum :: Type) (redeemer :: Type) = TestValidator {getTestValidator :: Validator}
  deriving stock (Show)

newtype TestMintingPolicy (redeemer :: Type) = TestMintingPolicy {getTestMintingPolicy :: MintingPolicy}
  deriving stock (Show)

newtype WrappedValidator
  = WrappedValidator (BuiltinData -> BuiltinData -> BuiltinData -> ())

{-# INLINEABLE getWrappedValidator #-}
getWrappedValidator :: WrappedValidator -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
getWrappedValidator (WrappedValidator v) = v

newtype WrappedMintingPolicy
  = WrappedMintingPolicy (BuiltinData -> BuiltinData -> ())

{-# INLINEABLE getWrappedMintingPolicy #-}
getWrappedMintingPolicy :: WrappedMintingPolicy -> (BuiltinData -> BuiltinData -> ())
getWrappedMintingPolicy (WrappedMintingPolicy mp) = mp

mkTestValidator ::
  forall (datum :: Type) (redeemer :: Type) (ctx :: Type).
  CompiledCode (datum -> redeemer -> ctx -> Bool) ->
  CompiledCode ((datum -> redeemer -> ctx -> Bool) -> WrappedValidator) ->
  TestValidator datum redeemer
mkTestValidator v wr =
  TestValidator $
    mkValidatorScript $
      applyCode $$(compile [||getWrappedValidator||]) $
        wr `applyCode` v

mkTestValidatorUnsafe ::
  forall (d :: Type) (r :: Type) (datum :: Type) (redeemer :: Type) (ctx :: Type).
  CompiledCode (datum -> redeemer -> ctx -> Bool) ->
  CompiledCode ((datum -> redeemer -> ctx -> Bool) -> WrappedValidator) ->
  TestValidator d r
mkTestValidatorUnsafe v wr =
  TestValidator $
    mkValidatorScript $
      applyCode $$(compile [||getWrappedValidator||]) $
        wr `applyCode` v

mkTestMintingPolicy ::
  forall (redeemer :: Type) (ctx :: Type).
  CompiledCode (redeemer -> ctx -> Bool) ->
  CompiledCode ((redeemer -> ctx -> Bool) -> WrappedMintingPolicy) ->
  TestMintingPolicy redeemer
mkTestMintingPolicy mp wr =
  TestMintingPolicy $
    mkMintingPolicyScript $
      applyCode $$(compile [||getWrappedMintingPolicy||]) $
        wr `applyCode` mp

mkTestMintingPolicyUnsafe ::
  forall (r :: Type) (redeemer :: Type) (ctx :: Type).
  CompiledCode (redeemer -> ctx -> Bool) ->
  CompiledCode ((redeemer -> ctx -> Bool) -> WrappedMintingPolicy) ->
  TestMintingPolicy r
mkTestMintingPolicyUnsafe mp wr =
  TestMintingPolicy $
    mkMintingPolicyScript $
      applyCode $$(compile [||getWrappedMintingPolicy||]) $
        wr `applyCode` mp

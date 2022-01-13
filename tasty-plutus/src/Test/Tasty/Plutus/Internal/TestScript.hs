{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Plutus.Internal.TestScript (
  -- * Types
  TestValidator (..),
  TestMintingPolicy (..),
  WrappedValidator (..),
  WrappedMintingPolicy (..),

  -- * Utility functions
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

{- | Typed wrapper for the 'Validator' used to match the datum and redeemer types
 of the 'Validator' and the data passed to it.

 We don't expose the constructor. To create you need to use
 helper function 'mkTestValidator'. In case you intend to test something tricky,
 you can use 'mkTestValidatorUnsafe' to create a 'TestValidator' that accepts datum and/or
 redeemer inconsistent with its internal type.

 @since 6.0
-}
newtype TestValidator (datum :: Type) (redeemer :: Type) = TestValidator {getTestValidator :: Validator}
  deriving stock
    ( -- | @since 6.0
      Show
    )

{- | Typed wrapper for the 'MintingPolicy' used to match the type
 of the 'MintingPolicy' redeemer and the data passed to it.

 We don't expose the constructor. To create you need to use helper function 'mkMintingPolicy'.
 In case you intend to test something tricky, you can use 'mkTestValidatorUnsafe' to create
 a 'TestMintingPolicy' that accepts redeemer inconsistent with its internal type.

 @since 6.0
-}
newtype TestMintingPolicy (redeemer :: Type) = TestMintingPolicy {getTestMintingPolicy :: MintingPolicy}
  deriving stock
    ( -- | @since 6.0
      Show
    )

{- | A wrapper for an untyped 'Validator'.
 This is similar to 'WrappedValidatorType' from the Plutus 'Ledger.Typed.Scripts' module.
 It`s aim is to force you to use 'toTestValidator' to create a 'TestValidator'.

 @since 6.0
-}
newtype WrappedValidator
  = WrappedValidator (BuiltinData -> BuiltinData -> BuiltinData -> ())

{-# INLINEABLE getWrappedValidator #-}
getWrappedValidator :: WrappedValidator -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
getWrappedValidator (WrappedValidator v) = v

{- | A wrapper for an untyped 'MintingPolicy'.
 This is similar to 'WrappedMintingPolicyType' from the Plutus 'Ledger.Typed.Scripts' module.
 It`s aim is to force you to use 'toTestMintingPolicy' to create a 'TestMintingPolicy'.

 @since 6.0
-}
newtype WrappedMintingPolicy
  = WrappedMintingPolicy (BuiltinData -> BuiltinData -> ())

{-# INLINEABLE getWrappedMintingPolicy #-}
getWrappedMintingPolicy :: WrappedMintingPolicy -> (BuiltinData -> BuiltinData -> ())
getWrappedMintingPolicy (WrappedMintingPolicy mp) = mp

{- | 'mkTestValidator' provide the way for creating 'TestValidator', parameterized with
 proper datum and redeemer types.

  = Usage

 > testValidator :: TestValidator SomeType SomeOtherType
 > testValidator =
 >  mkTestValidator
 >    $$(compile [||myValidator||])
 >    $$(compile [||toTestValidator||])

 @since 6.0
-}
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

{- | 'mkTestValidatorUnsafe' provide the way for creating 'TestValidator', parameterized with
 any datum and redeemer types. You can use it in case you going to test something tricky
 and need to break types compatibility.

  = Usage

 > myValidator :: SomeDatumType -> SomeRedeemerType -> ScriptContext -> Bool
 > myValidator d r ctx = ...

 > testValidator :: TestValidator SomeOtherDatumType SomeOtherRedeemerType
 > testValidator =
 >  mkTestValidatorUnsafe
 >    $$(compile [||myValidator||])
 >    $$(compile [||toTestValidator||])

 @since 6.0
-}
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

{- | 'mkTestMintingPolicy' provide the way for creating 'TestMintingPolicy',
 parameterized with proper redeemer type.

  = Usage

 > testMintingPolicy :: TestMintingPolicy SomeType
 > testMintingPolicy =
 >  mkTestMintingPolicy
 >    $$(compile [||myMintingPolicy||])
 >    $$(compile [||toTestMintingPolicy||])

 @since 6.0
-}
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

{- | 'mkTestMintingPolicyUnsafe' provide the way for creating 'TestMintingPolicy', parameterized with
 any redeemer type. You can use it in case you going to test something tricky
 and need to break types compatibility.

  = Usage

 > myMintingPolicy :: SomeRedeemerType -> ScriptContext -> Bool
 > myMintingPolicy r ctx = ...

 > testMintingPolicy :: TestMintingPolicy SomeOtherRedeemerType
 > testMintingPolicy =
 >  mkTestMintingPolicyUnsafe
 >    $$(compile [||myMintingPolicy||])
 >    $$(compile [||toTestMintingPolicy||])

 @since 6.0
-}
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

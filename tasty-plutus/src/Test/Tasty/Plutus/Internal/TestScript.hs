{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Plutus.Internal.TestScript (
  -- * Types
  TestScript (..),
  WrappedValidator (..),
  WrappedMintingPolicy (..),

  -- * Utility functions
  mkTestValidator,
  mkTestValidatorUnsafe,
  mkTestMintingPolicy,
  mkTestMintingPolicyUnsafe,
  toTestMintingPolicy,
  toTestValidator,
) where

import Data.Kind (Type)
import Ledger.Typed.Scripts (Validator)
import Plutus.V1.Ledger.Api (
  MintingPolicy,
  mkMintingPolicyScript,
  mkValidatorScript,
 )
import PlutusTx (CompiledCode, applyCode)
import PlutusTx.Builtins (BuiltinData, BuiltinString, appendString)
import PlutusTx.IsData.Class (FromData (fromBuiltinData))
import PlutusTx.TH (compile)
import PlutusTx.Trace (traceError)
import Test.Plutus.ContextBuilder (Purpose (ForMinting, ForSpending))

{- | Typed wrapper for the 'Validator' and 'MintingPolicy' used to match
 the datum and redeemer types of the 'Validator' and the data passed to it.

 We don't expose constructors. To create a 'TestScript', use helper functions,
 such as 'mkTestValidator' and 'mkTestMintingPolicy'. In case you intend
 to test something tricky, you can use 'mkTestValidatorUnsafe'
 and 'mkTestMintingPolicyUnsafe' to create a 'TestScript'
 that accepts a datum and/or redeemer inconsistent with its internal type.

 @since 6.0
-}
data TestScript (p :: Purpose) where
  -- | since 6.0
  TestValidator ::
    forall (d :: Type) (r :: Type) (datum :: Type) (redeemer :: Type) (ctx :: Type).
    { getTestValidatorCode :: CompiledCode (datum -> redeemer -> ctx -> Bool)
    , getTestValidator :: Validator
    } ->
    TestScript ( 'ForSpending d r)
  -- | since 6.0
  TestMintingPolicy ::
    forall (r :: Type) (redeemer :: Type) (ctx :: Type).
    { getTestMintingPolicyCode :: CompiledCode (redeemer -> ctx -> Bool)
    , getTestMintingPolicy :: MintingPolicy
    } ->
    TestScript ( 'ForMinting r)

{- | A wrapper for an untyped 'Validator'. This is similar
 to 'WrappedValidatorType' from the Plutus 'Ledger.Typed.Scripts' module.
 Its aim is to force you to use 'toTestValidator' to create a 'TestValidator'.

 @since 7.2
-}
newtype WrappedValidator
  = WrappedValidator (BuiltinData -> BuiltinData -> BuiltinData -> ())

{-# INLINEABLE getWrappedValidator #-}
getWrappedValidator ::
  WrappedValidator -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
getWrappedValidator (WrappedValidator v) = v

{- | A wrapper for an untyped 'MintingPolicy'. This is similar
 to 'WrappedMintingPolicyType' from the Plutus 'Ledger.Typed.Scripts' module.
 Its aim is to force you to use 'toTestMintingPolicy' to create
 a 'TestMintingPolicy'.

 @since 7.2
-}
newtype WrappedMintingPolicy
  = WrappedMintingPolicy (BuiltinData -> BuiltinData -> ())

{-# INLINEABLE getWrappedMintingPolicy #-}
getWrappedMintingPolicy ::
  WrappedMintingPolicy -> (BuiltinData -> BuiltinData -> ())
getWrappedMintingPolicy (WrappedMintingPolicy mp) = mp

{- | Create a 'TestScript' with the given 'Validator', parameterized
 with proper datum and redeemer types.

  = Usage

 > testValidator :: TestScript ( 'ForSpending SomeType SomeOtherType)
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
  TestScript ( 'ForSpending datum redeemer)
mkTestValidator v wr =
  TestValidator v $
    mkValidatorScript $
      applyCode $$(compile [||getWrappedValidator||]) $
        wr `applyCode` v

{- | As 'mkTestValidator', but without any guarantees of consistency regarding
 datum and redeemer types. You can use this in case you need to test something
 tricky, and the 'mkTestValidator' restrictions make this impossible.

 = Note

 This is not safe and may result in an error when the script is run.

 = Usage

 > myValidator :: SomeDatumType -> SomeRedeemerType -> ScriptContext -> Bool
 > myValidator d r ctx = ...

 > testValidator ::
 >     TestScript ( 'ForSpending SomeOtherDatumType SomeOtherRedeemerType)
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
  TestScript ( 'ForSpending d r)
mkTestValidatorUnsafe v wr =
  TestValidator v $
    mkValidatorScript $
      applyCode $$(compile [||getWrappedValidator||]) $
        wr `applyCode` v

{- | Create a 'TestScript' with the given 'MintingPolicy', parameterized
 with proper redeemer type.

  = Usage

 > testMintingPolicy :: TestScript ( 'ForMinting SomeType)
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
  TestScript ( 'ForMinting redeemer)
mkTestMintingPolicy mp wr =
  TestMintingPolicy mp $
    mkMintingPolicyScript $
      applyCode $$(compile [||getWrappedMintingPolicy||]) $
        wr `applyCode` mp

{- | As 'mkTestMintingPolicy', but without any guarantees of consistency regarding
 redeemer type. You can use this in case you need to test something
 tricky, and the 'mkTestMintingPolicy' restrictions make this impossible.

 = Note

 This is not safe and may result in an error when the script is run.

  = Usage

 > myMintingPolicy :: SomeRedeemerType -> ScriptContext -> Bool
 > myMintingPolicy r ctx = ...

 > testMintingPolicy :: TestScript ( 'ForMinting SomeOtherRedeemerType)
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
  TestScript ( 'ForMinting r)
mkTestMintingPolicyUnsafe mp wr =
  TestMintingPolicy mp $
    mkMintingPolicyScript $
      applyCode $$(compile [||getWrappedMintingPolicy||]) $
        wr `applyCode` mp

{- | A wrapper for validators. Use this to construct @TestScript ForSpending@.

 = Usage

 > testValidator :: TestScript ( 'ForSpending SomeType SomeOtherType)
 > testValidator =
 >  mkTestValidator
 >    $$(compile [||myValidator||])
 >    $$(compile [||toTestValidator||])

 = Important note

 If @myValidator@ requires \'burned in\' arguments, these should be passed via
 'liftCode' and 'applyCode', rather than as literal arguments inside of
 'compile':

 > testValidatorWithArg ::
 >    ArgumentType ->
 >    TestScript ( 'ForSpending SomeType SomeOtherType)
 > testValidatorWithArg arg =
 >  mkTestValidator
 >    ( $$(compile [||myValidator||])
 >          `applyCode`
 >          liftCode arg
 >    )
 >    $$(compile [||toTestValidator||])

 @since 3.0
-}
{-# INLINEABLE toTestValidator #-}
toTestValidator ::
  forall (datum :: Type) (redeemer :: Type) (ctx :: Type).
  (FromData datum, FromData redeemer, FromData ctx) =>
  (datum -> redeemer -> ctx -> Bool) ->
  WrappedValidator
toTestValidator f = WrappedValidator $ \d r p ->
  case fromBuiltinData d of
    Nothing -> reportParseFailed "Datum"
    Just d' -> case fromBuiltinData r of
      Nothing -> reportParseFailed "Redeemer"
      Just r' -> case fromBuiltinData p of
        Nothing -> reportParseFailed "ScriptContext"
        Just p' ->
          if f d' r' p'
            then reportPass
            else reportFail

{- | A wrapper for minting policies. Use this to construct
 a @TestScript ForMinting@.

 The usage (and caveats) of this function is similar to 'toTestValidator'; see
 its documentation for details.

 = Usage

 > testMintingPolicy :: TestScript ( 'ForMinting SomeType)
 > testMintingPolicy =
 >  mkTestMintingPolicy
 >    $$(compile [||myMintingPolicy||])
 >    $$(compile [||toTestMintingPolicy||])

 @since 3.0
-}
{-# INLINEABLE toTestMintingPolicy #-}
toTestMintingPolicy ::
  forall (redeemer :: Type) (ctx :: Type).
  (FromData redeemer, FromData ctx) =>
  (redeemer -> ctx -> Bool) ->
  WrappedMintingPolicy
toTestMintingPolicy f = WrappedMintingPolicy $ \r p ->
  case fromBuiltinData r of
    Nothing -> reportParseFailed "Redeemer"
    Just r' -> case fromBuiltinData p of
      Nothing -> reportParseFailed "ScriptContext"
      Just p' ->
        if f r' p'
          then reportPass
          else reportFail

-- Helpers

{-# INLINEABLE reportParseFailed #-}
reportParseFailed :: BuiltinString -> ()
reportParseFailed what =
  traceError ("tasty-plutus: Parse failed: " `appendString` what)

{-# INLINEABLE reportPass #-}
reportPass :: ()
reportPass = ()

{-# INLINEABLE reportFail #-}
reportFail :: ()
reportFail = traceError "tasty-plutus: Fail"

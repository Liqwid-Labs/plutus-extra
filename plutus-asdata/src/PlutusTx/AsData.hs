{-# LANGUAGE TemplateHaskell #-}

module PlutusTx.AsData where

import Data.Kind (Type)
import Ledger (
  Address,
  Datum,
  PubKeyHash,
  ScriptContext,
  TxId,
  TxInInfo,
  TxInfo,
  TxOut,
  TxOutRef,
  Value,
 )
import Ledger.Typed.Scripts (WrappedValidatorType)
import PlutusTx (
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.AsData.Internal (AsData (AsData))
import PlutusTx.AsData.Internal.TH (mkAsDataAccessors)
import PlutusTx.Prelude
import Prelude ()

{-# INLINEABLE safeFromData #-}
safeFromData :: UnsafeFromData x => AsData x -> x
safeFromData (AsData d) = unsafeFromBuiltinData d

{-# INLINEABLE safeToData #-}
safeToData :: ToData x => x -> AsData x
safeToData x = AsData $ toBuiltinData x

{-# INLINEABLE unsafeInjectData #-}
unsafeInjectData :: BuiltinData -> AsData x
unsafeInjectData = AsData

{-# INLINEABLE forgetData #-}
forgetData :: AsData x -> BuiltinData
forgetData (AsData d) = d

{-# INLINEABLE inefficientMapData #-}
inefficientMapData :: (UnsafeFromData x, ToData y) => (x -> y) -> AsData x -> AsData y
inefficientMapData f x = safeToData $ f $ safeFromData x

mkAsDataAccessors ''ScriptContext
mkAsDataAccessors ''TxInfo
mkAsDataAccessors ''TxInInfo
mkAsDataAccessors ''TxOut
mkAsDataAccessors ''TxOutRef
mkAsDataAccessors ''TxId
mkAsDataAccessors ''Address
mkAsDataAccessors ''Value
mkAsDataAccessors ''PubKeyHash
mkAsDataAccessors ''Datum

{-# INLINEABLE toTypedValidator #-}
toTypedValidator ::
  forall (d :: Type) (r :: Type).
  (ToData d, ToData r) =>
  (AsData d -> AsData r -> AsData ScriptContext -> Bool) ->
  (d -> r -> ScriptContext -> Bool)
toTypedValidator f d r sc = f (safeToData d) (safeToData r) (safeToData sc)

{-# INLINEABLE toWrappedValidator #-}
toWrappedValidator ::
  forall (d :: Type) (r :: Type).
  (AsData d -> AsData r -> AsData ScriptContext -> Bool) ->
  WrappedValidatorType
toWrappedValidator f d r sc = check $ f (unsafeInjectData d) (unsafeInjectData r) (unsafeInjectData sc)

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusTx.Data.Extra (
  toDatum,
  toDatumHash,
  toRedeemer,
  fromDatum,
  fromRedeemer,
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import Ledger.Scripts (datumHash)
import Plutus.V1.Ledger.Api (
  Datum (Datum),
  DatumHash,
  FromData (fromBuiltinData),
  Redeemer (Redeemer),
  ToData (toBuiltinData),
 )
import PlutusTx.Prelude (Maybe, (.))

--------------------------------------------------------------------------------

{- | Construct Datum using ToData instance

 @since 4.1
-}
{-# INLINEABLE toDatum #-}
toDatum ::
  forall (datum :: Type).
  (ToData datum) =>
  datum ->
  Datum
toDatum = Datum . toBuiltinData

{- | Get DatumHash using ToData instance

 **Note**: not intended to be used on-chain

 @since 4.1
-}
{-# INLINEABLE toDatumHash #-}
toDatumHash ::
  forall (datum :: Type).
  (ToData datum) =>
  datum ->
  DatumHash
toDatumHash = datumHash . toDatum @datum

{- | Construct Redeemer using ToData instance

 @since 4.1
-}
{-# INLINEABLE toRedeemer #-}
toRedeemer ::
  forall (redeemer :: Type).
  (ToData redeemer) =>
  redeemer ->
  Redeemer
toRedeemer = Redeemer . toBuiltinData

{- | Parse Datum using FromData instance

 @since 4.1
-}
{-# INLINEABLE fromDatum #-}
fromDatum ::
  forall (datum :: Type).
  (FromData datum) =>
  Datum ->
  Maybe datum
fromDatum (Datum d) = fromBuiltinData d

{- | Parse Redeemer using FromData instance

 @since 4.1
-}
{-# INLINEABLE fromRedeemer #-}
fromRedeemer ::
  forall (redeemer :: Type).
  (FromData redeemer) =>
  Redeemer ->
  Maybe redeemer
fromRedeemer (Redeemer d) = fromBuiltinData d

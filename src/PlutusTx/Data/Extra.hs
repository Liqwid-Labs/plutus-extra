{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusTx.Data.Extra (
  toDatum,
  toRedeemer,
  fromDatum,
  fromRedeemer,
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api qualified as Ledger (Datum (..), Redeemer (..))
import PlutusTx.IsData qualified as Ledger
import PlutusTx.Prelude (Maybe, (.))

--------------------------------------------------------------------------------

-- | Converts an arbitrary value to a `Datum`
{-# INLINEABLE toDatum #-}
toDatum ::
  forall (script :: Type).
  (Ledger.ToData (Scripts.DatumType script)) =>
  Scripts.DatumType script ->
  Ledger.Datum
toDatum = Ledger.Datum . Ledger.toBuiltinData

-- | Converts an arbitrary value to a `Redeemer`
{-# INLINEABLE toRedeemer #-}
toRedeemer ::
  forall (script :: Type).
  (Ledger.ToData (Scripts.RedeemerType script)) =>
  Scripts.RedeemerType script ->
  Ledger.Redeemer
toRedeemer = Ledger.Redeemer . Ledger.toBuiltinData

-- | Convert a `Datum` back to an ordinary Haskell value of the correct type
{-# INLINEABLE fromDatum #-}
fromDatum ::
  forall (script :: Type).
  (Ledger.FromData (Scripts.DatumType script)) =>
  Ledger.Datum ->
  Maybe (Scripts.DatumType script)
fromDatum (Ledger.Datum d) = Ledger.fromBuiltinData d

-- | Convert a `Redeemer` back to an ordinary Haskell value of the correct type
{-# INLINEABLE fromRedeemer #-}
fromRedeemer ::
  forall (script :: Type).
  (Ledger.FromData (Scripts.RedeemerType script)) =>
  Ledger.Redeemer ->
  Maybe (Scripts.RedeemerType script)
fromRedeemer (Ledger.Redeemer d) = Ledger.fromBuiltinData d

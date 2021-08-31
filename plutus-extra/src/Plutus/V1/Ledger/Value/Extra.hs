{-# OPTIONS_GHC -fno-omit-interface-pragmas -fno-specialize #-}

module Plutus.V1.Ledger.Value.Extra (
  assetClasses,
  filterValue,
  filterValueClass,
  valueIntersectEq,
  valueSubsetOf,
) where

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Value qualified as Ledger (Value (..))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude
import PlutusTx.These (these)

--------------------------------------------------------------------------------

import PlutusTx.PointFree.Extra ((&))
import PlutusTx.Tuple.Extra qualified as Tuple.Extra

--------------------------------------------------------------------------------

{-# INLINEABLE assetClasses #-}
assetClasses :: Ledger.Value -> [Value.AssetClass]
assetClasses val =
  (\(a, b, _) -> Value.AssetClass (a, b)) <$> Value.flattenValue val

{-# INLINEABLE filterValue #-}

-- | Variant of assetClassValueOf returning Value instead of Integer
filterValueClass :: Ledger.Value -> Value.AssetClass -> Ledger.Value
filterValueClass val asset =
  Value.assetClassValue asset $
    Value.assetClassValueOf val asset

-- | Filter Value with a predicate
filterValue :: (Value.CurrencySymbol -> Value.TokenName -> Integer -> Bool) -> Ledger.Value -> Ledger.Value
filterValue predicate v =
  filter (Tuple.Extra.uncurry3 predicate) (Value.flattenValue v)
    & foldMap (Tuple.Extra.uncurry3 Value.singleton)

{-# INLINEABLE valueIntersectEq #-}

-- | Returns True when the intersection of two `Value`s is equal
valueIntersectEq :: Ledger.Value -> Ledger.Value -> Bool
valueIntersectEq (Ledger.Value x) (Ledger.Value y) =
  AssocMap.all (these (const True) (const True) (==)) $ x `AssocMap.union` y

{-# INLINEABLE valueSubsetOf #-}

-- | Returns True when the left value is a subset of the right value
valueSubsetOf :: Ledger.Value -> Ledger.Value -> Bool
valueSubsetOf (Ledger.Value x) (Ledger.Value y) =
  AssocMap.all (these (const True) (const False) (==)) $ x `AssocMap.union` y

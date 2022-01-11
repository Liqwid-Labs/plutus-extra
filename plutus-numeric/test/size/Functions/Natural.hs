{-# LANGUAGE TemplateHaskell #-}

module Functions.Natural (
  natEq,
  natNeq,
  natCompare,
  natLE,
  natGE,
  natLT,
  natGT,
  natMin,
  natMax,
  natPlus,
  natZero,
  natMonus,
  natTimes,
  natOne,
  natToBuiltinData,
  natFromBuiltinData,
  natUnsafeFromBuiltinData,
  natPowNat,
  natDivMod,
  natScaleNat,
) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.IsData.Class (
  fromBuiltinData,
  toBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (
  divMod,
  powNat,
  (^-),
  scaleNat,
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.TH (compile)
import Prelude hiding (Rational, divMod, (/))

natEq :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natEq = $$(compile [||(Plutus.==)||])

natNeq :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natNeq = $$(compile [||(Plutus.==)||])

natCompare :: CompiledCode (Natural -> Natural -> Plutus.Ordering)
natCompare = $$(compile [||Plutus.compare||])

natLE :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natLE = $$(compile [||(Plutus.<=)||])

natGE :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natGE = $$(compile [||(Plutus.>=)||])

natLT :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natLT = $$(compile [||(Plutus.<)||])

natGT :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natGT = $$(compile [||(Plutus.>)||])

natMin :: CompiledCode (Natural -> Natural -> Natural)
natMin = $$(compile [||Plutus.min||])

natMax :: CompiledCode (Natural -> Natural -> Natural)
natMax = $$(compile [||Plutus.max||])

natPlus :: CompiledCode (Natural -> Natural -> Natural)
natPlus = $$(compile [||(Plutus.+)||])

natZero :: CompiledCode Natural
natZero = $$(compile [||Plutus.zero||])

natMonus :: CompiledCode (Natural -> Natural -> Natural)
natMonus = $$(compile [||(^-)||])

natTimes :: CompiledCode (Natural -> Natural -> Natural)
natTimes = $$(compile [||(Plutus.*)||])

natOne :: CompiledCode Natural
natOne = $$(compile [||Plutus.one||])

natToBuiltinData :: CompiledCode (Natural -> Plutus.BuiltinData)
natToBuiltinData = $$(compile [||toBuiltinData||])

natFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Maybe Natural)
natFromBuiltinData = $$(compile [||fromBuiltinData||])

natUnsafeFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Natural)
natUnsafeFromBuiltinData = $$(compile [||unsafeFromBuiltinData||])

natPowNat :: CompiledCode (Natural -> Natural -> Natural)
natPowNat = $$(compile [||powNat||])

natDivMod :: CompiledCode (Natural -> Natural -> (Natural, Natural))
natDivMod = $$(compile [||divMod||])

natScaleNat :: CompiledCode (Natural -> Natural -> Natural)
natScaleNat = $$(compile [||scaleNat||])
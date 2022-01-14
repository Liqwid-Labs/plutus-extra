{-# LANGUAGE TemplateHaskell #-}

module Functions.NatRatio (
  nrEq,
  nrNeq,
  nrCompare,
  nrLE,
  nrGE,
  nrLT,
  nrGT,
  nrMin,
  nrMax,
  nrPlus,
  nrZero,
  nrMonus,
  nrTimes,
  nrOne,
  nrDiv,
  nrRecip,
  nrPowInteger,
  nrFromNatural,
  nrNumerator,
  nrDenominator,
  nrRound,
  nrTruncate,
  nrProperFraction,
  nrToBuiltinData,
  nrFromBuiltinData,
  nrUnsafeFromBuiltinData,
  nrPowNat,
  nrScaleNat,
) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.IsData.Class (
  fromBuiltinData,
  toBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.NatRatio (NatRatio)
import PlutusTx.NatRatio qualified as NatRatio
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (
  powInteger,
  powNat,
  reciprocal,
  scaleNat,
  (/),
  (^-),
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.TH (compile)
import Prelude hiding (Rational, abs, signum, (/))

nrEq :: CompiledCode (NatRatio -> NatRatio -> Plutus.Bool)
nrEq = $$(compile [||(Plutus.==)||])

nrNeq :: CompiledCode (NatRatio -> NatRatio -> Plutus.Bool)
nrNeq = $$(compile [||(Plutus.==)||])

nrCompare :: CompiledCode (NatRatio -> NatRatio -> Plutus.Ordering)
nrCompare = $$(compile [||Plutus.compare||])

nrLE :: CompiledCode (NatRatio -> NatRatio -> Plutus.Bool)
nrLE = $$(compile [||(Plutus.<=)||])

nrGE :: CompiledCode (NatRatio -> NatRatio -> Plutus.Bool)
nrGE = $$(compile [||(Plutus.>=)||])

nrLT :: CompiledCode (NatRatio -> NatRatio -> Plutus.Bool)
nrLT = $$(compile [||(Plutus.<)||])

nrGT :: CompiledCode (NatRatio -> NatRatio -> Plutus.Bool)
nrGT = $$(compile [||(Plutus.>)||])

nrMin :: CompiledCode (NatRatio -> NatRatio -> NatRatio)
nrMin = $$(compile [||Plutus.min||])

nrMax :: CompiledCode (NatRatio -> NatRatio -> NatRatio)
nrMax = $$(compile [||Plutus.max||])

nrPlus :: CompiledCode (NatRatio -> NatRatio -> NatRatio)
nrPlus = $$(compile [||(Plutus.+)||])

nrZero :: CompiledCode NatRatio
nrZero = $$(compile [||Plutus.zero||])

nrMonus :: CompiledCode (NatRatio -> NatRatio -> NatRatio)
nrMonus = $$(compile [||(^-)||])

nrTimes :: CompiledCode (NatRatio -> NatRatio -> NatRatio)
nrTimes = $$(compile [||(Plutus.*)||])

nrOne :: CompiledCode NatRatio
nrOne = $$(compile [||Plutus.one||])

nrDiv :: CompiledCode (NatRatio -> NatRatio -> NatRatio)
nrDiv = $$(compile [||(/)||])

nrRecip :: CompiledCode (NatRatio -> NatRatio)
nrRecip = $$(compile [||reciprocal||])

nrPowInteger :: CompiledCode (NatRatio -> Plutus.Integer -> NatRatio)
nrPowInteger = $$(compile [||powInteger||])

nrFromNatural :: CompiledCode (Natural -> NatRatio)
nrFromNatural = $$(compile [||NatRatio.fromNatural||])

nrNumerator :: CompiledCode (NatRatio -> Natural)
nrNumerator = $$(compile [||NatRatio.numerator||])

nrDenominator :: CompiledCode (NatRatio -> Natural)
nrDenominator = $$(compile [||NatRatio.denominator||])

nrRound :: CompiledCode (NatRatio -> Natural)
nrRound = $$(compile [||NatRatio.round||])

nrTruncate :: CompiledCode (NatRatio -> Natural)
nrTruncate = $$(compile [||NatRatio.truncate||])

nrProperFraction :: CompiledCode (NatRatio -> (Natural, NatRatio))
nrProperFraction = $$(compile [||NatRatio.properFraction||])

nrToBuiltinData :: CompiledCode (NatRatio -> Plutus.BuiltinData)
nrToBuiltinData = $$(compile [||toBuiltinData||])

nrFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Maybe NatRatio)
nrFromBuiltinData = $$(compile [||fromBuiltinData||])

nrUnsafeFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> NatRatio)
nrUnsafeFromBuiltinData = $$(compile [||unsafeFromBuiltinData||])

nrPowNat :: CompiledCode (NatRatio -> Natural -> NatRatio)
nrPowNat = $$(compile [||powNat||])

nrScaleNat :: CompiledCode (Natural -> NatRatio -> NatRatio)
nrScaleNat = $$(compile [||scaleNat||])

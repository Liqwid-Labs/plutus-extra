{-# LANGUAGE TemplateHaskell #-}

module Functions.PTxRational (
  rEq,
  rNeq,
  rCompare,
  rLeq,
  rGeq,
  rLt,
  rGt,
  rMin,
  rMax,
  rPlus,
  rZero,
  rMinus,
  rNegate,
  rTimes,
  rOne,
  rFromInteger,
  rNumerator,
  rDenominator,
  rRound,
  rTruncate,
  rProperFrac,
  rRecip,
  rAbs,
  rHalf,
  rScale,
  rDiv,
  rPowNat,
  rPowInteger,
  rToBuiltinData,
  rFromBuiltinData,
  rUnsafeFromBuiltinData,
  rProperFraction,
) where

import PlutusTx.IsData.Class (fromBuiltinData, toBuiltinData, unsafeFromBuiltinData)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.Ratio as PlutusRatio
import PlutusTx.TH (compile)
import PlutusTx.Numeric.Extra qualified as PlutusNum
import PlutusTx.Natural (Natural)

rEq :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
rEq = $$(compile [||(Plutus.==)||])

rNeq :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
rNeq = $$(compile [||(Plutus./=)||])

rCompare :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Ordering)
rCompare = $$(compile [||Plutus.compare||])

rLeq :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
rLeq = $$(compile [||(Plutus.<=)||])

rGeq :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
rGeq = $$(compile [||(Plutus.>=)||])

rLt :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
rLt = $$(compile [||(Plutus.<)||])

rGt :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
rGt = $$(compile [||(Plutus.>)||])

rMin :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
rMin = $$(compile [||Plutus.min||])

rMax :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
rMax = $$(compile [||Plutus.max||])

rPlus :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
rPlus = $$(compile [||(Plutus.+)||])

rZero :: CompiledCode Plutus.Rational
rZero = $$(compile [||Plutus.zero||])

rMinus :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
rMinus = $$(compile [||(Plutus.-)||])

rNegate :: CompiledCode (Plutus.Rational -> Plutus.Rational)
rNegate = $$(compile [||Plutus.negate||])

rTimes :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
rTimes = $$(compile [||(Plutus.*)||])

rOne :: CompiledCode Plutus.Rational
rOne = $$(compile [||Plutus.one||])

rFromInteger :: CompiledCode (Integer -> Plutus.Rational)
rFromInteger = $$(compile [||Plutus.fromInteger||])

rNumerator :: CompiledCode (Plutus.Rational -> Integer)
rNumerator = $$(compile [||PlutusRatio.numerator||])

rDenominator :: CompiledCode (Plutus.Rational -> Integer)
rDenominator = $$(compile [||PlutusRatio.denominator||])

rRound :: CompiledCode (Plutus.Rational -> Integer)
rRound = $$(compile [||PlutusRatio.round||])

rTruncate :: CompiledCode (Plutus.Rational -> Integer)
rTruncate = $$(compile [||PlutusRatio.truncate||])

rProperFrac :: CompiledCode (Plutus.Rational -> (Integer, Plutus.Rational))
rProperFrac = $$(compile [||PlutusRatio.properFraction||])

rRecip :: CompiledCode (Plutus.Rational -> Plutus.Rational)
rRecip = $$(compile [||PlutusRatio.recip||])

rAbs :: CompiledCode (Plutus.Rational -> Plutus.Rational)
rAbs = $$(compile [||PlutusRatio.abs||])

rHalf :: CompiledCode Plutus.Rational
rHalf = $$(compile [||PlutusRatio.half||])

rScale :: CompiledCode (Integer -> Plutus.Rational -> Plutus.Rational)
rScale = $$(compile [||Plutus.scale||])

rDiv :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
rDiv = $$(compile [||(PlutusNum./)||])

rPowNat :: CompiledCode (Plutus.Rational -> Natural -> Plutus.Rational)
rPowNat = $$(compile [||PlutusNum.powNat||])

rPowInteger :: CompiledCode (Plutus.Rational -> Integer -> Plutus.Rational)
rPowInteger = $$(compile [||PlutusNum.powInteger||])

rToBuiltinData :: CompiledCode (Plutus.Rational -> Plutus.BuiltinData)
rToBuiltinData = $$(compile [||toBuiltinData||])

rFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Maybe Plutus.Rational)
rFromBuiltinData = $$(compile [||fromBuiltinData||])

rUnsafeFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Rational)
rUnsafeFromBuiltinData = $$(compile [||unsafeFromBuiltinData||])

rProperFraction :: CompiledCode (Plutus.Rational -> (Integer, Plutus.Rational))
rProperFraction = $$(compile [||PlutusRatio.properFraction||])

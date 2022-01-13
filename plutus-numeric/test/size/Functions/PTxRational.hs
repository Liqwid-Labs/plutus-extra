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
) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.Ratio as PlutusRatio
import PlutusTx.TH (compile)

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

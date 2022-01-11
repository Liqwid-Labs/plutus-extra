{-# LANGUAGE TemplateHaskell #-}

module Functions.Rational (
  rEq,
  rNeq,
  rCompare,
  rLE,
  rGE,
  rLT,
  rGT,
  rMin,
  rMax,
  rPlus,
  rZero,
  rMinus,
  rNegate,
  rTimes,
  rOne,
  rDiv,
  rRecip,
  rPowInteger,
  rFromInteger,
  rNumerator,
  rDenominator,
  rRound,
  rTruncate,
  rProperFraction,
  rToBuiltinData,
  rFromBuiltinData,
  rUnsafeFromBuiltinData,
  rAbs,
  rProjectAbs,
  rAddExtend,
  rRestrictMay,
  rSignum,
  rPowNat,
  rLeq,
  rGeq,
  rLt,
  rGt,
  rNegateOverload,
  rProperFrac,
  rAbsOverload,
  rHalf,
  rScaleNat,
) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.IsData.Class (
  fromBuiltinData,
  toBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.NatRatio (NatRatio)
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (
  abs,
  addExtend,
  powInteger,
  powNat,
  projectAbs,
  reciprocal,
  restrictMay,
  signum,
  scaleNat,
  (/),
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.Rational (Rational, half, negate, properFraction)
import PlutusTx.Rational qualified as Rational
import PlutusTx.TH (compile)
import Prelude hiding (Rational, abs, negate, properFraction, signum, (/))

rEq :: CompiledCode (Rational -> Rational -> Plutus.Bool)
rEq = $$(compile [||(Plutus.==)||])

rNeq :: CompiledCode (Rational -> Rational -> Plutus.Bool)
rNeq = $$(compile [||(Plutus.==)||])

rCompare :: CompiledCode (Rational -> Rational -> Plutus.Ordering)
rCompare = $$(compile [||Plutus.compare||])

rLE :: CompiledCode (Rational -> Rational -> Plutus.Bool)
rLE = $$(compile [||(Plutus.<=)||])

rGE :: CompiledCode (Rational -> Rational -> Plutus.Bool)
rGE = $$(compile [||(Plutus.>=)||])

rLT :: CompiledCode (Rational -> Rational -> Plutus.Bool)
rLT = $$(compile [||(Plutus.<)||])

rGT :: CompiledCode (Rational -> Rational -> Plutus.Bool)
rGT = $$(compile [||(Plutus.>)||])

rMin :: CompiledCode (Rational -> Rational -> Rational)
rMin = $$(compile [||Plutus.min||])

rMax :: CompiledCode (Rational -> Rational -> Rational)
rMax = $$(compile [||Plutus.max||])

rPlus :: CompiledCode (Rational -> Rational -> Rational)
rPlus = $$(compile [||(Plutus.+)||])

rZero :: CompiledCode Rational
rZero = $$(compile [||Plutus.zero||])

rMinus :: CompiledCode (Rational -> Rational -> Rational)
rMinus = $$(compile [||(Plutus.-)||])

rNegate :: CompiledCode (Rational -> Rational)
rNegate = $$(compile [||Rational.negate||])

rTimes :: CompiledCode (Rational -> Rational -> Rational)
rTimes = $$(compile [||(Plutus.*)||])

rOne :: CompiledCode Rational
rOne = $$(compile [||Plutus.one||])

rDiv :: CompiledCode (Rational -> Rational -> Rational)
rDiv = $$(compile [||(/)||])

rRecip :: CompiledCode (Rational -> Rational)
rRecip = $$(compile [||reciprocal||])

rPowInteger :: CompiledCode (Rational -> Plutus.Integer -> Rational)
rPowInteger = $$(compile [||powInteger||])

rFromInteger :: CompiledCode (Plutus.Integer -> Rational)
rFromInteger = $$(compile [||Rational.fromInteger||])

rNumerator :: CompiledCode (Rational -> Integer)
rNumerator = $$(compile [||Rational.numerator||])

rDenominator :: CompiledCode (Rational -> Integer)
rDenominator = $$(compile [||Rational.denominator||])

rRound :: CompiledCode (Rational -> Integer)
rRound = $$(compile [||Rational.round||])

rTruncate :: CompiledCode (Rational -> Integer)
rTruncate = $$(compile [||Rational.truncate||])

rProperFraction :: CompiledCode (Rational -> (Integer, Rational))
rProperFraction = $$(compile [||Rational.properFraction||])

rToBuiltinData :: CompiledCode (Rational -> Plutus.BuiltinData)
rToBuiltinData = $$(compile [||toBuiltinData||])

rFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Maybe Rational)
rFromBuiltinData = $$(compile [||fromBuiltinData||])

rUnsafeFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Rational)
rUnsafeFromBuiltinData = $$(compile [||unsafeFromBuiltinData||])

rAbs :: CompiledCode (Rational -> Rational)
rAbs = $$(compile [||abs||])

rProjectAbs :: CompiledCode (Rational -> NatRatio)
rProjectAbs = $$(compile [||projectAbs||])

rAddExtend :: CompiledCode (NatRatio -> Rational)
rAddExtend = $$(compile [||addExtend||])

rRestrictMay :: CompiledCode (Rational -> Maybe NatRatio)
rRestrictMay = $$(compile [||restrictMay||])

rSignum :: CompiledCode (Rational -> Rational)
rSignum = $$(compile [||signum||])

rPowNat :: CompiledCode (Rational -> Natural -> Rational)
rPowNat = $$(compile [||powNat||])

rLeq :: CompiledCode (Rational -> Rational -> Plutus.Bool)
rLeq = $$(compile [||(Plutus.<=)||])

rGeq :: CompiledCode (Rational -> Rational -> Plutus.Bool)
rGeq = $$(compile [||(Plutus.>=)||])

rLt :: CompiledCode (Rational -> Rational -> Plutus.Bool)
rLt = $$(compile [||(Plutus.<)||])

rGt :: CompiledCode (Rational -> Rational -> Plutus.Bool)
rGt = $$(compile [||(Plutus.>)||])

rNegateOverload :: CompiledCode (Rational -> Rational)
rNegateOverload = $$(compile [||negate||])

rProperFrac :: CompiledCode (Rational -> (Integer, Rational))
rProperFrac = $$(compile [||properFraction||])

rAbsOverload :: CompiledCode (Rational -> Rational)
rAbsOverload = $$(compile [||abs||])

rHalf :: CompiledCode Rational
rHalf = $$(compile [||half||])

rScaleNat :: CompiledCode (Natural -> Rational -> Rational)
rScaleNat = $$(compile [||scaleNat||])
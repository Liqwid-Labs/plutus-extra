{-# LANGUAGE TemplateHaskell #-}

module Functions.Integer (
  iEq,
  iNeq,
  iCompare,
  iLE,
  iGE,
  iLT,
  iGT,
  iMin,
  iMax,
  iPlus,
  iZero,
  iMinus,
  iTimes,
  iOne,
  iToBuiltinData,
  iFromBuiltinData,
  iUnsafeFromBuiltinData,
  iPowNat,
  iDivMod,
  iAbs,
  iProjectAbs,
  iAddExtend,
  iRestrictMay,
  iSignum,
) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.IsData.Class (
  fromBuiltinData,
  toBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (
  abs,
  addExtend,
  divMod,
  powNat,
  projectAbs,
  restrictMay,
  signum,
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.TH (compile)
import Prelude hiding (Rational, abs, divMod, signum, (/))

iEq :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iEq = $$(compile [||(Plutus.==)||])

iNeq :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iNeq = $$(compile [||(Plutus.==)||])

iCompare :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Ordering)
iCompare = $$(compile [||Plutus.compare||])

iLE :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iLE = $$(compile [||(Plutus.<=)||])

iGE :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iGE = $$(compile [||(Plutus.>=)||])

iLT :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iLT = $$(compile [||(Plutus.<)||])

iGT :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iGT = $$(compile [||(Plutus.>)||])

iMin :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iMin = $$(compile [||Plutus.min||])

iMax :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iMax = $$(compile [||Plutus.max||])

iPlus :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iPlus = $$(compile [||(Plutus.+)||])

iZero :: CompiledCode Plutus.Integer
iZero = $$(compile [||Plutus.zero||])

iMinus :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iMinus = $$(compile [||(Plutus.-)||])

iTimes :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iTimes = $$(compile [||(Plutus.*)||])

iOne :: CompiledCode Plutus.Integer
iOne = $$(compile [||Plutus.one||])

iToBuiltinData :: CompiledCode (Plutus.Integer -> Plutus.BuiltinData)
iToBuiltinData = $$(compile [||toBuiltinData||])

iFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Maybe Plutus.Integer)
iFromBuiltinData = $$(compile [||fromBuiltinData||])

iUnsafeFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Integer)
iUnsafeFromBuiltinData = $$(compile [||unsafeFromBuiltinData||])

iPowNat :: CompiledCode (Plutus.Integer -> Natural -> Plutus.Integer)
iPowNat = $$(compile [||powNat||])

iDivMod :: CompiledCode (Plutus.Integer -> Plutus.Integer -> (Plutus.Integer, Plutus.Integer))
iDivMod = $$(compile [||divMod||])

iAbs :: CompiledCode (Plutus.Integer -> Plutus.Integer)
iAbs = $$(compile [||abs||])

iProjectAbs :: CompiledCode (Plutus.Integer -> Natural)
iProjectAbs = $$(compile [||projectAbs||])

iAddExtend :: CompiledCode (Natural -> Plutus.Integer)
iAddExtend = $$(compile [||addExtend||])

iRestrictMay :: CompiledCode (Plutus.Integer -> Maybe Natural)
iRestrictMay = $$(compile [||restrictMay||])

iSignum :: CompiledCode (Plutus.Integer -> Plutus.Integer)
iSignum = $$(compile [||signum||])

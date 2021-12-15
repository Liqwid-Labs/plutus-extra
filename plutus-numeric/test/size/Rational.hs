{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Rational (tests) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
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
  (/),
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.Rational (Rational)
import PlutusTx.Rational qualified as Rational
import PlutusTx.TH (compile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Size (bytes, fitsInto)
import Prelude hiding (Rational, abs, signum, (/))

tests :: [TestTree]
tests =
  [ testGroup
      "Eq"
      [ fitsInto "==" [bytes| 72 |] . fromCompiledCode $ rEq
      , fitsInto "/=" [bytes| 72 |] . fromCompiledCode $ rNeq
      ]
  , testGroup
      "Ord"
      [ fitsInto "compare" [bytes| 109 |] . fromCompiledCode $ rCompare
      , fitsInto "<=" [bytes| 62 |] . fromCompiledCode $ rLE
      , fitsInto ">=" [bytes| 62 |] . fromCompiledCode $ rGE
      , fitsInto "<" [bytes| 62 |] . fromCompiledCode $ rLT
      , fitsInto ">" [bytes| 62 |] . fromCompiledCode $ rGT
      , fitsInto "min" [bytes| 70 |] . fromCompiledCode $ rMin
      , fitsInto "max" [bytes| 70 |] . fromCompiledCode $ rMax
      ]
  , testGroup
      "AdditiveGroup"
      [ fitsInto "+" [bytes| 159 |] . fromCompiledCode $ rPlus
      , fitsInto "zero" [bytes| 24 |] . fromCompiledCode $ rZero
      , fitsInto "-" [bytes| 159 |] . fromCompiledCode $ rMinus
      , fitsInto "negate" [bytes| 35 |] . fromCompiledCode $ rNegate
      ]
  , testGroup
      "MultiplicativeGroup"
      [ fitsInto "*" [bytes| 151 |] . fromCompiledCode $ rTimes
      , fitsInto "one" [bytes| 24 |] . fromCompiledCode $ rOne
      , fitsInto "/" [bytes| 144 |] . fromCompiledCode $ rDiv
      , fitsInto "reciprocal" [bytes| 92 |] . fromCompiledCode $ rRecip
      , fitsInto "powNat" [bytes| 343 |] . fromCompiledCode $ rPowNat
      , fitsInto "powInteger" [bytes| 357 |] . fromCompiledCode $ rPowInteger
      ]
  , testGroup
      "IntegralDomain"
      [ fitsInto "abs" [bytes| 69 |] . fromCompiledCode $ rAbs
      , fitsInto "projectAbs" [bytes| 69 |] . fromCompiledCode $ rProjectAbs
      , fitsInto "addExtend" [bytes| 19 |] . fromCompiledCode $ rAddExtend
      , fitsInto "restrictMay" [bytes| 95 |] . fromCompiledCode $ rRestrictMay
      , fitsInto "signum" [bytes| 97 |] . fromCompiledCode $ rSignum
      ]
  , testGroup
      "Serialization"
      [ fitsInto "toBuiltinData" [bytes| 78 |] . fromCompiledCode $ rToBuiltinData
      , fitsInto "fromBuiltinData" [bytes| 398 |] . fromCompiledCode $ rFromBuiltinData
      , fitsInto "unsafeFromBuiltinData" [bytes| 263 |] . fromCompiledCode $ rUnsafeFromBuiltinData
      ]
  , testGroup
      "Other"
      [ fitsInto "fromInteger" [bytes| 24 |] . fromCompiledCode $ rFromInteger
      , fitsInto "numerator" [bytes| 26 |] . fromCompiledCode $ rNumerator
      , fitsInto "denominator" [bytes| 26 |] . fromCompiledCode $ rNumerator
      , fitsInto "round" [bytes| 342 |] . fromCompiledCode $ rRound
      , fitsInto "truncate" [bytes| 30 |] . fromCompiledCode $ rTruncate
      , fitsInto "properFraction" [bytes| 57 |] . fromCompiledCode $ rProperFraction
      ]
  ]

-- Compiled code

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

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
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
  reciprocal,
  (/),
  (^-),
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.Rational (Rational)
import PlutusTx.Rational qualified as Rational
import PlutusTx.TH (compile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Plutus.Size (fitsUnder)
import Prelude hiding (Rational, (/))

main :: IO ()
main =
  defaultMain . testGroup "NatRatio vs Rational" $
    [ testGroup
        "Eq"
        [ fitsUnder "==" (fromCompiledCode nrEq) (fromCompiledCode rEq)
        , fitsUnder "/=" (fromCompiledCode nrNeq) (fromCompiledCode rNeq)
        ]
    , testGroup
        "Ord"
        [ fitsUnder "compare" (fromCompiledCode nrCompare) (fromCompiledCode rCompare)
        , fitsUnder "<=" (fromCompiledCode nrLE) (fromCompiledCode rLE)
        , fitsUnder ">=" (fromCompiledCode nrGE) (fromCompiledCode rGE)
        , fitsUnder "<" (fromCompiledCode nrLT) (fromCompiledCode rLT)
        , fitsUnder ">" (fromCompiledCode nrGT) (fromCompiledCode rGT)
        , fitsUnder "min" (fromCompiledCode nrMin) (fromCompiledCode rMin)
        , fitsUnder "max" (fromCompiledCode nrMax) (fromCompiledCode rMax)
        ]
    , testGroup
        "Additive"
        [ fitsUnder "+" (fromCompiledCode nrPlus) (fromCompiledCode rPlus)
        , fitsUnder "zero" (fromCompiledCode nrZero) (fromCompiledCode rZero)
        , expectFailBecause "monus is trickier"
            . fitsUnder "^- vs -" (fromCompiledCode nrMonus)
            . fromCompiledCode
            $ rMinus
        ]
    , testGroup
        "Multiplicative"
        [ fitsUnder "*" (fromCompiledCode nrTimes) (fromCompiledCode rTimes)
        , fitsUnder "one" (fromCompiledCode nrOne) (fromCompiledCode rOne)
        , fitsUnder "/" (fromCompiledCode nrDiv) (fromCompiledCode rDiv)
        , fitsUnder "reciprocal" (fromCompiledCode nrRecip) (fromCompiledCode rRecip)
        , fitsUnder "powInteger" (fromCompiledCode nrPowInteger) (fromCompiledCode rPowInteger)
        ]
    , testGroup
        "Serialization"
        [ fitsUnder
            "toBuiltinData"
            (fromCompiledCode nrToBuiltinData)
            (fromCompiledCode rToBuiltinData)
        , expectFailBecause "extra checks are required"
            . fitsUnder "fromBuiltinData" (fromCompiledCode nrFromBuiltinData)
            . fromCompiledCode
            $ rFromBuiltinData
        , fitsUnder
            "unsafeFromBuiltinData"
            (fromCompiledCode nrUnsafeFromBuiltinData)
            (fromCompiledCode rUnsafeFromBuiltinData)
        ]
    , testGroup
        "Other"
        [ fitsUnder
            "whole number conversion"
            (fromCompiledCode nrFromNatural)
            (fromCompiledCode rFromInteger)
        , fitsUnder
            "numerator"
            (fromCompiledCode nrNumerator)
            (fromCompiledCode rNumerator)
        , fitsUnder
            "denominator"
            (fromCompiledCode nrDenominator)
            (fromCompiledCode rNumerator)
        , fitsUnder
            "round"
            (fromCompiledCode nrRound)
            (fromCompiledCode rRound)
        , fitsUnder
            "truncate"
            (fromCompiledCode nrTruncate)
            (fromCompiledCode rTruncate)
        , fitsUnder
            "properFraction"
            (fromCompiledCode nrProperFraction)
            (fromCompiledCode rProperFraction)
        ]
    ]

-- Helpers

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

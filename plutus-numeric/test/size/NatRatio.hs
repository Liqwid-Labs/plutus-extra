{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module NatRatio (tests) where

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
  powNat,
  reciprocal,
  (/),
  (^-),
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.TH (compile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Size (bytes, fitsInto)
import Prelude hiding (Rational, abs, signum, (/))

tests :: [TestTree]
tests =
  [ testGroup
      "Eq"
      [ fitsInto "==" [bytes| 72 |] . fromCompiledCode $ nrEq
      , fitsInto "/=" [bytes| 72 |] . fromCompiledCode $ nrNeq
      ]
  , testGroup
      "Ord"
      [ fitsInto "compare" [bytes| 109 |] . fromCompiledCode $ nrCompare
      , fitsInto "<=" [bytes| 62 |] . fromCompiledCode $ nrLE
      , fitsInto ">=" [bytes| 62 |] . fromCompiledCode $ nrGE
      , fitsInto "<" [bytes| 62 |] . fromCompiledCode $ nrLT
      , fitsInto ">" [bytes| 62 |] . fromCompiledCode $ nrGT
      , fitsInto "min" [bytes| 70 |] . fromCompiledCode $ nrMin
      , fitsInto "max" [bytes| 70 |] . fromCompiledCode $ nrMax
      ]
  , testGroup
      "AdditiveGroup"
      [ fitsInto "+" [bytes| 159 |] . fromCompiledCode $ nrPlus
      , fitsInto "zero" [bytes| 24 |] . fromCompiledCode $ nrZero
      , fitsInto "^-" [bytes| 183 |] . fromCompiledCode $ nrMonus
      ]
  , testGroup
      "MultiplicativeGroup"
      [ fitsInto "*" [bytes| 151 |] . fromCompiledCode $ nrTimes
      , fitsInto "one" [bytes| 24 |] . fromCompiledCode $ nrOne
      , fitsInto "/" [bytes| 144 |] . fromCompiledCode $ nrDiv
      , fitsInto "reciprocal" [bytes| 92 |] . fromCompiledCode $ nrRecip
      , fitsInto "powNat" [bytes| 343 |] . fromCompiledCode $ nrPowNat
      , fitsInto "powInteger" [bytes| 357 |] . fromCompiledCode $ nrPowInteger
      ]
  , testGroup
      "Serialization"
      [ fitsInto "toBuiltinData" [bytes| 78 |] . fromCompiledCode $ nrToBuiltinData
      , fitsInto "fromBuiltinData" [bytes| 413 |] . fromCompiledCode $ nrFromBuiltinData
      , fitsInto "unsafeFromBuiltinData" [bytes| 263 |] . fromCompiledCode $ nrUnsafeFromBuiltinData
      ]
  , testGroup
      "Other"
      [ fitsInto "fromNatural" [bytes| 24 |] . fromCompiledCode $ nrFromNatural
      , fitsInto "numerator" [bytes| 26 |] . fromCompiledCode $ nrNumerator
      , fitsInto "denominator" [bytes| 26 |] . fromCompiledCode $ nrNumerator
      , fitsInto "round" [bytes| 342 |] . fromCompiledCode $ nrRound
      , fitsInto "truncate" [bytes| 30 |] . fromCompiledCode $ nrTruncate
      , fitsInto "properFraction" [bytes| 57 |] . fromCompiledCode $ nrProperFraction
      ]
  ]

-- Compiled code

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

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Rational (tests) where

import Functions.Rational qualified as R
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Size (bytes, fitsInto)
import Prelude hiding (Rational, abs, signum, (/))

tests :: [TestTree]
tests =
  [ testGroup
      "Eq"
      [ fitsInto "==" [bytes| 72 |] . fromCompiledCode $ R.rEq
      , fitsInto "/=" [bytes| 72 |] . fromCompiledCode $ R.rNeq
      ]
  , testGroup
      "Ord"
      [ fitsInto "compare" [bytes| 109 |] . fromCompiledCode $ R.rCompare
      , fitsInto "<=" [bytes| 62 |] . fromCompiledCode $ R.rLE
      , fitsInto ">=" [bytes| 62 |] . fromCompiledCode $ R.rGE
      , fitsInto "<" [bytes| 62 |] . fromCompiledCode $ R.rLT
      , fitsInto ">" [bytes| 62 |] . fromCompiledCode $ R.rGT
      , fitsInto "min" [bytes| 70 |] . fromCompiledCode $ R.rMin
      , fitsInto "max" [bytes| 70 |] . fromCompiledCode $ R.rMax
      ]
  , testGroup
      "AdditiveGroup"
      [ fitsInto "+" [bytes| 159 |] . fromCompiledCode $ R.rPlus
      , fitsInto "zero" [bytes| 24 |] . fromCompiledCode $ R.rZero
      , fitsInto "-" [bytes| 159 |] . fromCompiledCode $ R.rMinus
      , fitsInto "negate" [bytes| 35 |] . fromCompiledCode $ R.rNegate
      ]
  , testGroup
      "MultiplicativeGroup"
      [ fitsInto "*" [bytes| 151 |] . fromCompiledCode $ R.rTimes
      , fitsInto "one" [bytes| 24 |] . fromCompiledCode $ R.rOne
      , fitsInto "/" [bytes| 144 |] . fromCompiledCode $ R.rDiv
      , fitsInto "reciprocal" [bytes| 92 |] . fromCompiledCode $ R.rRecip
      , fitsInto "powNat" [bytes| 343 |] . fromCompiledCode $ R.rPowNat
      , fitsInto "powInteger" [bytes| 357 |] . fromCompiledCode $ R.rPowInteger
      ]
  , testGroup
      "IntegralDomain"
      [ fitsInto "abs" [bytes| 69 |] . fromCompiledCode $ R.rAbs
      , fitsInto "projectAbs" [bytes| 69 |] . fromCompiledCode $ R.rProjectAbs
      , fitsInto "addExtend" [bytes| 19 |] . fromCompiledCode $ R.rAddExtend
      , fitsInto "restrictMay" [bytes| 95 |] . fromCompiledCode $ R.rRestrictMay
      , fitsInto "signum" [bytes| 97 |] . fromCompiledCode $ R.rSignum
      ]
  , testGroup
      "Serialization"
      [ fitsInto "toBuiltinData" [bytes| 78 |] . fromCompiledCode $ R.rToBuiltinData
      , fitsInto "fromBuiltinData" [bytes| 390 |] . fromCompiledCode $ R.rFromBuiltinData
      , fitsInto "unsafeFromBuiltinData" [bytes| 263 |] . fromCompiledCode $ R.rUnsafeFromBuiltinData
      ]
  , testGroup
      "Other"
      [ fitsInto "fromInteger" [bytes| 24 |] . fromCompiledCode $ R.rFromInteger
      , fitsInto "numerator" [bytes| 26 |] . fromCompiledCode $ R.rNumerator
      , fitsInto "denominator" [bytes| 26 |] . fromCompiledCode $ R.rNumerator
      , fitsInto "round" [bytes| 342 |] . fromCompiledCode $ R.rRound
      , fitsInto "truncate" [bytes| 30 |] . fromCompiledCode $ R.rTruncate
      , fitsInto "properFraction" [bytes| 57 |] . fromCompiledCode $ R.rProperFraction
      ]
  ]

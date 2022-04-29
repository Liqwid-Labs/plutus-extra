module Compare.NatRatio (tests) where

import Functions.NatRatio qualified as NR
import Functions.PTxRational qualified as R
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Plutus.Size (fitsUnder)
import Prelude hiding (Rational, (/))

tests :: [TestTree]
tests =
  [ testGroup
      "Eq"
      [ fitsUnder "==" (fromCompiledCode NR.nrEq) (fromCompiledCode R.rEq)
      , fitsUnder "/=" (fromCompiledCode NR.nrNeq) (fromCompiledCode R.rNeq)
      ]
  , testGroup
      "Ord"
      [ fitsUnder "compare" (fromCompiledCode NR.nrCompare) (fromCompiledCode R.rCompare)
      , fitsUnder "<=" (fromCompiledCode NR.nrLE) (fromCompiledCode R.rLeq)
      , fitsUnder ">=" (fromCompiledCode NR.nrGE) (fromCompiledCode R.rGeq)
      , fitsUnder "<" (fromCompiledCode NR.nrLT) (fromCompiledCode R.rLt)
      , fitsUnder ">" (fromCompiledCode NR.nrGT) (fromCompiledCode R.rGt)
      , fitsUnder "min" (fromCompiledCode NR.nrMin) (fromCompiledCode R.rMin)
      , fitsUnder "max" (fromCompiledCode NR.nrMax) (fromCompiledCode R.rMax)
      ]
  , testGroup
      "Additive"
      [ fitsUnder "+" (fromCompiledCode NR.nrPlus) (fromCompiledCode R.rPlus)
      , fitsUnder "zero" (fromCompiledCode NR.nrZero) (fromCompiledCode R.rZero)
      , fitsUnder "scale" (fromCompiledCode NR.nrSemiscale) (fromCompiledCode R.rScale)
      , expectFailBecause "monus is trickier"
          . fitsUnder "^- vs -" (fromCompiledCode NR.nrMonus)
          . fromCompiledCode
          $ R.rMinus
      ]
  , testGroup
      "Multiplicative"
      [ fitsUnder "*" (fromCompiledCode NR.nrTimes) (fromCompiledCode R.rTimes)
      , fitsUnder "one" (fromCompiledCode NR.nrOne) (fromCompiledCode R.rOne)
      , fitsUnder "/" (fromCompiledCode NR.nrDiv) (fromCompiledCode R.rDiv)
      , fitsUnder "reciprocal" (fromCompiledCode NR.nrRecip) (fromCompiledCode R.rRecip)
      , fitsUnder "powNat" (fromCompiledCode NR.nrPowNat) (fromCompiledCode R.rPowNat)
      , fitsUnder "powInteger" (fromCompiledCode NR.nrPowInteger) (fromCompiledCode R.rPowInteger)
      ]
  , testGroup
      "Serialization"
      [ fitsUnder
          "toBuiltinData"
          (fromCompiledCode NR.nrToBuiltinData)
          (fromCompiledCode R.rToBuiltinData)
      , expectFailBecause "extra checks are required"
          . fitsUnder "fromBuiltinData" (fromCompiledCode NR.nrFromBuiltinData)
          . fromCompiledCode
          $ R.rFromBuiltinData
      , fitsUnder
          "unsafeFromBuiltinData"
          (fromCompiledCode NR.nrUnsafeFromBuiltinData)
          (fromCompiledCode R.rUnsafeFromBuiltinData)
      ]
  , testGroup
      "Other"
      [ fitsUnder
          "whole number conversion"
          (fromCompiledCode NR.nrFromNatural)
          (fromCompiledCode R.rFromInteger)
      , fitsUnder
          "numerator"
          (fromCompiledCode NR.nrNumerator)
          (fromCompiledCode R.rNumerator)
      , fitsUnder
          "denominator"
          (fromCompiledCode NR.nrDenominator)
          (fromCompiledCode R.rNumerator)
      , fitsUnder
          "round"
          (fromCompiledCode NR.nrRound)
          (fromCompiledCode R.rRound)
      , fitsUnder
          "truncate"
          (fromCompiledCode NR.nrTruncate)
          (fromCompiledCode R.rTruncate)
      , fitsUnder
          "properFraction"
          (fromCompiledCode NR.nrProperFraction)
          (fromCompiledCode R.rProperFraction)
      ]
  ]

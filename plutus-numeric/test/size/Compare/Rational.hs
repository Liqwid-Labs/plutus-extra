module Compare.Rational (tests) where

import Functions.PTxRational qualified as PTx
import Functions.Rational qualified as R
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Size (fitsUnder)

tests :: [TestTree]
tests =
  [ testGroup
      "Eq"
      [ fitsUnder "==" (fromCompiledCode R.rEq) (fromCompiledCode PTx.rEq)
      , fitsUnder "/=" (fromCompiledCode R.rNeq) (fromCompiledCode PTx.rNeq)
      ]
  , testGroup
      "Ord"
      [ fitsUnder "compare" (fromCompiledCode R.rCompare) (fromCompiledCode PTx.rCompare)
      , fitsUnder "<=" (fromCompiledCode R.rLeq) (fromCompiledCode PTx.rLeq)
      , fitsUnder ">=" (fromCompiledCode R.rGeq) (fromCompiledCode PTx.rGeq)
      , fitsUnder "<" (fromCompiledCode R.rLt) (fromCompiledCode PTx.rLt)
      , fitsUnder ">" (fromCompiledCode R.rGt) (fromCompiledCode PTx.rGt)
      , fitsUnder "min" (fromCompiledCode R.rMin) (fromCompiledCode PTx.rMin)
      , fitsUnder "max" (fromCompiledCode R.rMax) (fromCompiledCode PTx.rMax)
      ]
  , testGroup
      "AdditiveGroup"
      [ fitsUnder "+" (fromCompiledCode R.rPlus) (fromCompiledCode PTx.rPlus)
      , fitsUnder "zero" (fromCompiledCode R.rZero) (fromCompiledCode PTx.rZero)
      , fitsUnder "-" (fromCompiledCode R.rMinus) (fromCompiledCode PTx.rMinus)
      , fitsUnder "negate" (fromCompiledCode R.rNegate) (fromCompiledCode PTx.rNegate)
      , fitsUnder
          "negate (overloaded)"
          (fromCompiledCode R.rNegateOverload)
          (fromCompiledCode PTx.rNegate)
      ]
  , testGroup
      "MultiplicativeMonoid"
      [ fitsUnder "*" (fromCompiledCode R.rTimes) (fromCompiledCode PTx.rTimes)
      , fitsUnder "one" (fromCompiledCode R.rOne) (fromCompiledCode PTx.rOne)
      ]
  , testGroup
      "Other"
      [ fitsUnder
          "fromInteger"
          (fromCompiledCode R.rFromInteger)
          (fromCompiledCode PTx.rFromInteger)
      , fitsUnder
          "numerator"
          (fromCompiledCode R.rNumerator)
          (fromCompiledCode PTx.rNumerator)
      , fitsUnder
          "denominator"
          (fromCompiledCode R.rDenominator)
          (fromCompiledCode PTx.rDenominator)
      , fitsUnder
          "round"
          (fromCompiledCode R.rRound)
          (fromCompiledCode PTx.rRound)
      , fitsUnder
          "truncate"
          (fromCompiledCode R.rTruncate)
          (fromCompiledCode PTx.rTruncate)
      , fitsUnder
          "properFraction"
          (fromCompiledCode R.rProperFrac)
          (fromCompiledCode PTx.rProperFrac)
      , fitsUnder
          "recip"
          (fromCompiledCode R.rRecip)
          (fromCompiledCode PTx.rRecip)
      , fitsUnder
          "abs"
          (fromCompiledCode R.rAbs)
          (fromCompiledCode PTx.rAbs)
      , fitsUnder
          "abs (overloaded)"
          (fromCompiledCode R.rAbsOverload)
          (fromCompiledCode PTx.rAbs)
      , fitsUnder
          "half"
          (fromCompiledCode R.rHalf)
          (fromCompiledCode PTx.rHalf)
      ]
  ]

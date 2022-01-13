module Compare.Natural (tests) where

import Functions.Integer qualified as I
import Functions.Natural qualified as Nat
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Plutus.Size (fitsUnder)
import Prelude hiding (Rational, divMod, (/))

tests :: [TestTree]
tests =
  [ testGroup
      "Eq"
      [ fitsUnder "==" (fromCompiledCode Nat.natEq) (fromCompiledCode I.iEq)
      , fitsUnder "/=" (fromCompiledCode Nat.natNeq) (fromCompiledCode I.iNeq)
      ]
  , testGroup
      "Ord"
      [ fitsUnder "compare" (fromCompiledCode Nat.natCompare) (fromCompiledCode I.iCompare)
      , fitsUnder "<=" (fromCompiledCode Nat.natLE) (fromCompiledCode I.iLE)
      , fitsUnder ">=" (fromCompiledCode Nat.natGE) (fromCompiledCode I.iGE)
      , fitsUnder "<" (fromCompiledCode Nat.natLT) (fromCompiledCode I.iLT)
      , fitsUnder ">" (fromCompiledCode Nat.natGT) (fromCompiledCode I.iGT)
      , fitsUnder "min" (fromCompiledCode Nat.natMin) (fromCompiledCode I.iMin)
      , fitsUnder "max" (fromCompiledCode Nat.natMax) (fromCompiledCode I.iMax)
      ]
  , testGroup
      "Additive"
      [ fitsUnder "+" (fromCompiledCode Nat.natPlus) (fromCompiledCode I.iPlus)
      , fitsUnder "zero" (fromCompiledCode Nat.natZero) (fromCompiledCode I.iZero)
      , fitsUnder "scaleNat" (fromCompiledCode Nat.natScaleNat) (fromCompiledCode I.iScaleNat)
      , expectFailBecause "monus requires more checks"
          . fitsUnder "^- vs -" (fromCompiledCode Nat.natMonus)
          . fromCompiledCode
          $ I.iMinus
      ]
  , testGroup
      "Multiplicative"
      [ fitsUnder "*" (fromCompiledCode Nat.natTimes) (fromCompiledCode I.iTimes)
      , fitsUnder "one" (fromCompiledCode Nat.natOne) (fromCompiledCode I.iOne)
      , fitsUnder "powNat" (fromCompiledCode Nat.natPowNat) (fromCompiledCode I.iPowNat)
      ]
  , testGroup
      "EuclideanClosed"
      [ fitsUnder "divMod" (fromCompiledCode Nat.natDivMod) (fromCompiledCode I.iDivMod)
      ]
  , testGroup
      "Serialization"
      [ fitsUnder
          "toBuiltinData"
          (fromCompiledCode Nat.natToBuiltinData)
          (fromCompiledCode I.iToBuiltinData)
      , expectFailBecause "Natural requires more checks"
          . fitsUnder "fromBuiltinData" (fromCompiledCode Nat.natFromBuiltinData)
          . fromCompiledCode
          $ I.iFromBuiltinData
      , expectFailBecause "Natural requires more checks"
          . fitsUnder "unsafeFromBuiltinData" (fromCompiledCode Nat.natUnsafeFromBuiltinData)
          . fromCompiledCode
          $ I.iUnsafeFromBuiltinData
      ]
  ]

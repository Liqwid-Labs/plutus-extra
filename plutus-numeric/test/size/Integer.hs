{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Integer (tests) where

import Functions.Integer qualified as I
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Size (bytes, fitsInto)
import Prelude hiding (Rational, abs, divMod, signum, (/))

tests :: [TestTree]
tests =
  [ testGroup
      "Eq"
      [ fitsInto "==" [bytes| 31 |] . fromCompiledCode $ I.iEq
      , fitsInto "/=" [bytes| 31 |] . fromCompiledCode $ I.iNeq
      ]
  , testGroup
      "Ord"
      [ fitsInto "compare" [bytes| 74 |] . fromCompiledCode $ I.iCompare
      , fitsInto "<=" [bytes| 31 |] . fromCompiledCode $ I.iLE
      , fitsInto ">=" [bytes| 31 |] . fromCompiledCode $ I.iGE
      , fitsInto "<" [bytes| 31 |] . fromCompiledCode $ I.iLT
      , fitsInto ">" [bytes| 31 |] . fromCompiledCode $ I.iGT
      , fitsInto "min" [bytes| 39 |] . fromCompiledCode $ I.iMin
      , fitsInto "max" [bytes| 39 |] . fromCompiledCode $ I.iMax
      ]
  , testGroup
      "AdditiveGroup"
      [ fitsInto "+" [bytes| 12 |] . fromCompiledCode $ I.iPlus
      , fitsInto "zero" [bytes| 8 |] . fromCompiledCode $ I.iZero
      , fitsInto "-" [bytes| 12 |] . fromCompiledCode $ I.iMinus
      , fitsInto "scaleNat" [bytes| 109 |] .fromCompiledCode $ I.iScaleNat
      ]
  , testGroup
      "MultiplicativeMonoid"
      [ fitsInto "*" [bytes| 12 |] . fromCompiledCode $ I.iTimes
      , fitsInto "one" [bytes| 8 |] . fromCompiledCode $ I.iOne
      , fitsInto "powNat" [bytes| 223 |] . fromCompiledCode $ I.iPowNat
      ]
  , testGroup
      "EuclideanClosed"
      [ fitsInto "divMod" [bytes| 74 |] . fromCompiledCode $ I.iDivMod
      ]
  , testGroup
      "IntegralDomain"
      [ fitsInto "abs" [bytes| 46 |] . fromCompiledCode $ I.iAbs
      , fitsInto "projectAbs" [bytes| 46 |] . fromCompiledCode $ I.iProjectAbs
      , fitsInto "addExtend" [bytes| 8 |] . fromCompiledCode $ I.iAddExtend
      , fitsInto "restrictMay" [bytes| 58 |] . fromCompiledCode $ I.iRestrictMay
      , fitsInto "signum" [bytes| 67 |] . fromCompiledCode $ I.iSignum
      ]
  , testGroup
      "Serialization"
      [ fitsInto "toBuiltinData" [bytes| 9 |] . fromCompiledCode $ I.iToBuiltinData
      , fitsInto "fromBuiltinData" [bytes| 54 |] . fromCompiledCode $ I.iFromBuiltinData
      , fitsInto "unsafeFromBuiltinData" [bytes| 7 |] . fromCompiledCode $ I.iUnsafeFromBuiltinData
      ]
  ]

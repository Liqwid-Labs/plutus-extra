{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Natural (tests) where

import Functions.Natural qualified as Nat
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Size (bytes, fitsInto)
import Prelude hiding (Rational, divMod, (/))

tests :: [TestTree]
tests =
  [ testGroup
      "Eq"
      [ fitsInto "==" [bytes| 31 |] . fromCompiledCode $ Nat.natEq
      , fitsInto "/=" [bytes| 31 |] . fromCompiledCode $ Nat.natNeq
      ]
  , testGroup
      "Ord"
      [ fitsInto "compare" [bytes| 74 |] . fromCompiledCode $ Nat.natCompare
      , fitsInto "<=" [bytes| 31 |] . fromCompiledCode $ Nat.natLE
      , fitsInto ">=" [bytes| 31 |] . fromCompiledCode $ Nat.natGE
      , fitsInto "<" [bytes| 31 |] . fromCompiledCode $ Nat.natLT
      , fitsInto ">" [bytes| 31 |] . fromCompiledCode $ Nat.natGT
      , fitsInto "min" [bytes| 39 |] . fromCompiledCode $ Nat.natMin
      , fitsInto "max" [bytes| 39 |] . fromCompiledCode $ Nat.natMax
      ]
  , testGroup
      "AdditiveHemigroup"
      [ fitsInto "+" [bytes| 12 |] . fromCompiledCode $ Nat.natPlus
      , fitsInto "zero" [bytes| 8 |] . fromCompiledCode $ Nat.natZero
      , fitsInto "^-" [bytes| 44 |] . fromCompiledCode $ Nat.natMonus
      , fitsInto "scaleNat" [bytes| 216 |] . fromCompiledCode $ Nat.natScaleNat
      ]
  , testGroup
      "MultiplicativeMonoid"
      [ fitsInto "*" [bytes| 12 |] . fromCompiledCode $ Nat.natTimes
      , fitsInto "one" [bytes| 8 |] . fromCompiledCode $ Nat.natOne
      , fitsInto "powNat" [bytes| 223 |] . fromCompiledCode $ Nat.natPowNat
      ]
  , testGroup
      "EuclideanClosed"
      [ fitsInto "divMod" [bytes| 74 |] . fromCompiledCode $ Nat.natDivMod
      ]
  , testGroup
      "Serialization"
      [ fitsInto "toBuiltinData" [bytes| 9 |] . fromCompiledCode $ Nat.natToBuiltinData
      , fitsInto "fromBuiltinData" [bytes| 89 |] . fromCompiledCode $ Nat.natFromBuiltinData
      , fitsInto "unsafeFromBuiltinData" [bytes| 44 |] . fromCompiledCode $ Nat.natUnsafeFromBuiltinData
      ]
  ]

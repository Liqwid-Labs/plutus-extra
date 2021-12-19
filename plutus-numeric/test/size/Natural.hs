{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Natural (tests) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.IsData.Class (
  fromBuiltinData,
  toBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (
  divMod,
  powNat,
  (^-),
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.TH (compile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Size (bytes, fitsInto)
import Prelude hiding (Rational, divMod, (/))

tests :: [TestTree]
tests =
  [ testGroup
      "Eq"
      [ fitsInto "==" [bytes| 31 |] . fromCompiledCode $ natEq
      , fitsInto "/=" [bytes| 31 |] . fromCompiledCode $ natNeq
      ]
  , testGroup
      "Ord"
      [ fitsInto "compare" [bytes| 74 |] . fromCompiledCode $ natCompare
      , fitsInto "<=" [bytes| 31 |] . fromCompiledCode $ natLE
      , fitsInto ">=" [bytes| 31 |] . fromCompiledCode $ natGE
      , fitsInto "<" [bytes| 31 |] . fromCompiledCode $ natLT
      , fitsInto ">" [bytes| 31 |] . fromCompiledCode $ natGT
      , fitsInto "min" [bytes| 39 |] . fromCompiledCode $ natMin
      , fitsInto "max" [bytes| 39 |] . fromCompiledCode $ natMax
      ]
  , testGroup
      "AdditiveHemigroup"
      [ fitsInto "+" [bytes| 12 |] . fromCompiledCode $ natPlus
      , fitsInto "zero" [bytes| 8 |] . fromCompiledCode $ natZero
      , fitsInto "^-" [bytes| 44 |] . fromCompiledCode $ natMonus
      ]
  , testGroup
      "MultiplicativeMonoid"
      [ fitsInto "*" [bytes| 12 |] . fromCompiledCode $ natTimes
      , fitsInto "one" [bytes| 8 |] . fromCompiledCode $ natOne
      , fitsInto "powNat" [bytes| 216 |] . fromCompiledCode $ natPowNat
      ]
  , testGroup
      "EuclideanClosed"
      [ fitsInto "divMod" [bytes| 72 |] . fromCompiledCode $ natDivMod
      ]
  , testGroup
      "Serialization"
      [ fitsInto "toBuiltinData" [bytes| 9 |] . fromCompiledCode $ natToBuiltinData
      , fitsInto "fromBuiltinData" [bytes| 89 |] . fromCompiledCode $ natFromBuiltinData
      , fitsInto "unsafeFromBuiltinData" [bytes| 44 |] . fromCompiledCode $ natUnsafeFromBuiltinData
      ]
  ]

-- Helpers

natEq :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natEq = $$(compile [||(Plutus.==)||])

natNeq :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natNeq = $$(compile [||(Plutus.==)||])

natCompare :: CompiledCode (Natural -> Natural -> Plutus.Ordering)
natCompare = $$(compile [||Plutus.compare||])

natLE :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natLE = $$(compile [||(Plutus.<=)||])

natGE :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natGE = $$(compile [||(Plutus.>=)||])

natLT :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natLT = $$(compile [||(Plutus.<)||])

natGT :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natGT = $$(compile [||(Plutus.>)||])

natMin :: CompiledCode (Natural -> Natural -> Natural)
natMin = $$(compile [||Plutus.min||])

natMax :: CompiledCode (Natural -> Natural -> Natural)
natMax = $$(compile [||Plutus.max||])

natPlus :: CompiledCode (Natural -> Natural -> Natural)
natPlus = $$(compile [||(Plutus.+)||])

natZero :: CompiledCode Natural
natZero = $$(compile [||Plutus.zero||])

natMonus :: CompiledCode (Natural -> Natural -> Natural)
natMonus = $$(compile [||(^-)||])

natTimes :: CompiledCode (Natural -> Natural -> Natural)
natTimes = $$(compile [||(Plutus.*)||])

natOne :: CompiledCode Natural
natOne = $$(compile [||Plutus.one||])

natToBuiltinData :: CompiledCode (Natural -> Plutus.BuiltinData)
natToBuiltinData = $$(compile [||toBuiltinData||])

natFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Maybe Natural)
natFromBuiltinData = $$(compile [||fromBuiltinData||])

natUnsafeFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Natural)
natUnsafeFromBuiltinData = $$(compile [||unsafeFromBuiltinData||])

natPowNat :: CompiledCode (Natural -> Natural -> Natural)
natPowNat = $$(compile [||powNat||])

natDivMod :: CompiledCode (Natural -> Natural -> (Natural, Natural))
natDivMod = $$(compile [||divMod||])

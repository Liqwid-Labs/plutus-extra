{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Integer (tests) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.IsData.Class (
  fromBuiltinData,
  toBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (
  abs,
  addExtend,
  divMod,
  powNat,
  projectAbs,
  restrictMay,
  signum,
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.TH (compile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Plutus.Size (bytes, fitsInto)
import Prelude hiding (Rational, abs, divMod, signum, (/))

tests :: [TestTree]
tests =
  [ testGroup
      "Eq"
      [ fitsInto "==" [bytes| 31 |] . fromCompiledCode $ iEq
      , fitsInto "/=" [bytes| 31 |] . fromCompiledCode $ iNeq
      ]
  , testGroup
      "Ord"
      [ fitsInto "compare" [bytes| 74 |] . fromCompiledCode $ iCompare
      , fitsInto "<=" [bytes| 31 |] . fromCompiledCode $ iLE
      , fitsInto ">=" [bytes| 31 |] . fromCompiledCode $ iGE
      , fitsInto "<" [bytes| 31 |] . fromCompiledCode $ iLT
      , fitsInto ">" [bytes| 31 |] . fromCompiledCode $ iGT
      , fitsInto "min" [bytes| 39 |] . fromCompiledCode $ iMin
      , fitsInto "max" [bytes| 39 |] . fromCompiledCode $ iMax
      ]
  , testGroup
      "AdditiveGroup"
      [ fitsInto "+" [bytes| 12 |] . fromCompiledCode $ iPlus
      , fitsInto "zero" [bytes| 8 |] . fromCompiledCode $ iZero
      , fitsInto "-" [bytes| 12 |] . fromCompiledCode $ iMinus
      ]
  , testGroup
      "MultiplicativeMonoid"
      [ fitsInto "*" [bytes| 12 |] . fromCompiledCode $ iTimes
      , fitsInto "one" [bytes| 8 |] . fromCompiledCode $ iOne
      , fitsInto "powNat" [bytes| 216 |] . fromCompiledCode $ iPowNat
      ]
  , testGroup
      "EuclideanClosed"
      [ fitsInto "divMod" [bytes| 72 |] . fromCompiledCode $ iDivMod
      ]
  , testGroup
      "IntegralDomain"
      [ fitsInto "abs" [bytes| 44 |] . fromCompiledCode $ iAbs
      , fitsInto "projectAbs" [bytes| 44 |] . fromCompiledCode $ iProjectAbs
      , fitsInto "addExtend" [bytes| 8 |] . fromCompiledCode $ iAddExtend
      , fitsInto "restrictMay" [bytes| 58 |] . fromCompiledCode $ iRestrictMay
      , fitsInto "signum" [bytes| 65 |] . fromCompiledCode $ iSignum
      ]
  , testGroup
      "Serialization"
      [ fitsInto "toBuiltinData" [bytes| 9 |] . fromCompiledCode $ iToBuiltinData
      , fitsInto "fromBuiltinData" [bytes| 54 |] . fromCompiledCode $ iFromBuiltinData
      , fitsInto "unsafeFromBuiltinData" [bytes| 7 |] . fromCompiledCode $ iUnsafeFromBuiltinData
      ]
  ]

-- Helpers

iEq :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iEq = $$(compile [||(Plutus.==)||])

iNeq :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iNeq = $$(compile [||(Plutus.==)||])

iCompare :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Ordering)
iCompare = $$(compile [||Plutus.compare||])

iLE :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iLE = $$(compile [||(Plutus.<=)||])

iGE :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iGE = $$(compile [||(Plutus.>=)||])

iLT :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iLT = $$(compile [||(Plutus.<)||])

iGT :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iGT = $$(compile [||(Plutus.>)||])

iMin :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iMin = $$(compile [||Plutus.min||])

iMax :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iMax = $$(compile [||Plutus.max||])

iPlus :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iPlus = $$(compile [||(Plutus.+)||])

iZero :: CompiledCode Plutus.Integer
iZero = $$(compile [||Plutus.zero||])

iMinus :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iMinus = $$(compile [||(Plutus.-)||])

iTimes :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iTimes = $$(compile [||(Plutus.*)||])

iOne :: CompiledCode Plutus.Integer
iOne = $$(compile [||Plutus.one||])

iToBuiltinData :: CompiledCode (Plutus.Integer -> Plutus.BuiltinData)
iToBuiltinData = $$(compile [||toBuiltinData||])

iFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Maybe Plutus.Integer)
iFromBuiltinData = $$(compile [||fromBuiltinData||])

iUnsafeFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Integer)
iUnsafeFromBuiltinData = $$(compile [||unsafeFromBuiltinData||])

iPowNat :: CompiledCode (Plutus.Integer -> Natural -> Plutus.Integer)
iPowNat = $$(compile [||powNat||])

iDivMod :: CompiledCode (Plutus.Integer -> Plutus.Integer -> (Plutus.Integer, Plutus.Integer))
iDivMod = $$(compile [||divMod||])

iAbs :: CompiledCode (Plutus.Integer -> Plutus.Integer)
iAbs = $$(compile [||abs||])

iProjectAbs :: CompiledCode (Plutus.Integer -> Natural)
iProjectAbs = $$(compile [||projectAbs||])

iAddExtend :: CompiledCode (Natural -> Plutus.Integer)
iAddExtend = $$(compile [||addExtend||])

iRestrictMay :: CompiledCode (Plutus.Integer -> Maybe Natural)
iRestrictMay = $$(compile [||restrictMay||])

iSignum :: CompiledCode (Plutus.Integer -> Plutus.Integer)
iSignum = $$(compile [||signum||])

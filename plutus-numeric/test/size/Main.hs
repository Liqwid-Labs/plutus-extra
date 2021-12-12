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
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (
  abs,
  addExtend,
  divMod,
  monus,
  powInteger,
  powNat,
  projectAbs,
  reciprocal,
  restrictMay,
  signum,
  (/),
 )
import PlutusTx.Prelude (BuiltinData)
import PlutusTx.Prelude qualified as PTx
import PlutusTx.Rational (Rational)
import PlutusTx.TH (compile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Size (fitsOnChain)
import Prelude hiding (Rational, abs, divMod, signum, (/))

main :: IO ()
main =
  defaultMain . testGroup "Size checks" $
    [ testGroup
        "Natural"
        [ fitsOnChain "==" . fromCompiledCode $ naturalEq
        , fitsOnChain "compare" . fromCompiledCode $ naturalCompare
        , fitsOnChain "+" . fromCompiledCode $ naturalPlus
        , fitsOnChain "zero" . fromCompiledCode $ naturalZero
        , fitsOnChain "*" . fromCompiledCode $ naturalTimes
        , fitsOnChain "one" . fromCompiledCode $ naturalOne
        , fitsOnChain "fromBuiltinData" . fromCompiledCode $ naturalFromBuiltinData
        , fitsOnChain "toBuiltinData" . fromCompiledCode $ naturalToBuiltinData
        , fitsOnChain "unsafeFromBuiltinData" . fromCompiledCode $ naturalUnsafeFromBuiltinData
        , fitsOnChain "powNat" . fromCompiledCode $ naturalPowNat
        , fitsOnChain "monus" . fromCompiledCode $ naturalMonus
        , fitsOnChain "divMod" . fromCompiledCode $ naturalDivMod
        ]
    , testGroup
        "NatRatio"
        [ fitsOnChain "==" . fromCompiledCode $ natRatioEq
        , fitsOnChain "compare" . fromCompiledCode $ natRatioCompare
        , fitsOnChain "+" . fromCompiledCode $ natRatioPlus
        , fitsOnChain "zero" . fromCompiledCode $ natRatioZero
        , fitsOnChain "*" . fromCompiledCode $ natRatioTimes
        , fitsOnChain "one" . fromCompiledCode $ natRatioOne
        , fitsOnChain "fromBuiltinData" . fromCompiledCode $ natRatioFromBuiltinData
        , fitsOnChain "toBuiltinData" . fromCompiledCode $ natRatioToBuiltinData
        , fitsOnChain "unsafeFromBuiltinData" . fromCompiledCode $ natRatioUnsafeFromBuiltinData
        , fitsOnChain "powNat" . fromCompiledCode $ natRatioPowNat
        , fitsOnChain "monus" . fromCompiledCode $ natRatioMonus
        , fitsOnChain "/" . fromCompiledCode $ natRatioDivide
        , fitsOnChain "reciprocal" . fromCompiledCode $ natRatioReciprocal
        , fitsOnChain "powInteger" . fromCompiledCode $ natRatioPowInteger
        ]
    , testGroup
        "Integer"
        [ fitsOnChain "divMod" . fromCompiledCode $ integerDivMod
        , fitsOnChain "abs" . fromCompiledCode $ integerAbs
        , fitsOnChain "projectAbs" . fromCompiledCode $ integerProjectAbs
        , fitsOnChain "addExtend" . fromCompiledCode $ integerAddExtend
        , fitsOnChain "restrictMay" . fromCompiledCode $ integerRestrictMay
        , fitsOnChain "signum" . fromCompiledCode $ integerSignum
        ]
    , testGroup
        "Rational"
        [ fitsOnChain "/" . fromCompiledCode $ rationalDivide
        , fitsOnChain "reciprocal" . fromCompiledCode $ rationalReciprocal
        , fitsOnChain "powInteger" . fromCompiledCode $ rationalPowInteger
        , fitsOnChain "abs" . fromCompiledCode $ rationalAbs
        , fitsOnChain "projectAbs" . fromCompiledCode $ rationalProjectAbs
        , fitsOnChain "addExtend" . fromCompiledCode $ rationalAddExtend
        , fitsOnChain "restrictMay" . fromCompiledCode $ rationalRestrictMay
        , fitsOnChain "signum" . fromCompiledCode $ rationalSignum
        ]
    ]

-- Compiled definitions

-- Natural

naturalEq :: CompiledCode (Natural -> Natural -> Bool)
naturalEq = $$(compile [||(PTx.==)||])

naturalCompare :: CompiledCode (Natural -> Natural -> Ordering)
naturalCompare = $$(compile [||PTx.compare||])

naturalPlus :: CompiledCode (Natural -> Natural -> Natural)
naturalPlus = $$(compile [||(PTx.+)||])

naturalZero :: CompiledCode Natural
naturalZero = $$(compile [||PTx.zero||])

naturalTimes :: CompiledCode (Natural -> Natural -> Natural)
naturalTimes = $$(compile [||(PTx.*)||])

naturalOne :: CompiledCode Natural
naturalOne = $$(compile [||PTx.one||])

naturalFromBuiltinData :: CompiledCode (BuiltinData -> Maybe Natural)
naturalFromBuiltinData = $$(compile [||fromBuiltinData||])

naturalToBuiltinData :: CompiledCode (Natural -> BuiltinData)
naturalToBuiltinData = $$(compile [||toBuiltinData||])

naturalUnsafeFromBuiltinData :: CompiledCode (BuiltinData -> Natural)
naturalUnsafeFromBuiltinData =
  $$(compile [||unsafeFromBuiltinData||])

naturalPowNat :: CompiledCode (Natural -> Natural -> Natural)
naturalPowNat = $$(compile [||powNat||])

naturalMonus :: CompiledCode (Natural -> Natural -> Natural)
naturalMonus = $$(compile [||monus||])

naturalDivMod :: CompiledCode (Natural -> Natural -> (Natural, Natural))
naturalDivMod = $$(compile [||divMod||])

-- NatRatio

natRatioEq :: CompiledCode (NatRatio -> NatRatio -> Bool)
natRatioEq = $$(compile [||(PTx.==)||])

natRatioCompare :: CompiledCode (NatRatio -> NatRatio -> Ordering)
natRatioCompare = $$(compile [||PTx.compare||])

natRatioPlus :: CompiledCode (NatRatio -> NatRatio -> NatRatio)
natRatioPlus = $$(compile [||(PTx.+)||])

natRatioZero :: CompiledCode NatRatio
natRatioZero = $$(compile [||PTx.zero||])

natRatioTimes :: CompiledCode (NatRatio -> NatRatio -> NatRatio)
natRatioTimes = $$(compile [||(PTx.*)||])

natRatioOne :: CompiledCode NatRatio
natRatioOne = $$(compile [||PTx.one||])

natRatioFromBuiltinData :: CompiledCode (BuiltinData -> Maybe NatRatio)
natRatioFromBuiltinData = $$(compile [||fromBuiltinData||])

natRatioToBuiltinData :: CompiledCode (NatRatio -> BuiltinData)
natRatioToBuiltinData = $$(compile [||toBuiltinData||])

natRatioUnsafeFromBuiltinData :: CompiledCode (BuiltinData -> NatRatio)
natRatioUnsafeFromBuiltinData =
  $$(compile [||unsafeFromBuiltinData||])

natRatioPowNat :: CompiledCode (NatRatio -> Natural -> NatRatio)
natRatioPowNat = $$(compile [||powNat||])

natRatioMonus :: CompiledCode (NatRatio -> NatRatio -> NatRatio)
natRatioMonus = $$(compile [||monus||])

natRatioDivide :: CompiledCode (NatRatio -> NatRatio -> NatRatio)
natRatioDivide = $$(compile [||(/)||])

natRatioReciprocal :: CompiledCode (NatRatio -> NatRatio)
natRatioReciprocal = $$(compile [||reciprocal||])

natRatioPowInteger :: CompiledCode (NatRatio -> Integer -> NatRatio)
natRatioPowInteger = $$(compile [||powInteger||])

-- Integer

integerDivMod :: CompiledCode (Integer -> Integer -> (Integer, Integer))
integerDivMod = $$(compile [||divMod||])

integerAbs :: CompiledCode (Integer -> Integer)
integerAbs = $$(compile [||abs||])

integerProjectAbs :: CompiledCode (Integer -> Natural)
integerProjectAbs = $$(compile [||projectAbs||])

integerAddExtend :: CompiledCode (Natural -> Integer)
integerAddExtend = $$(compile [||addExtend||])

integerRestrictMay :: CompiledCode (Integer -> Maybe Natural)
integerRestrictMay = $$(compile [||restrictMay||])

integerSignum :: CompiledCode (Integer -> Integer)
integerSignum = $$(compile [||signum||])

-- Rational

rationalDivide :: CompiledCode (Rational -> Rational -> Rational)
rationalDivide = $$(compile [||(/)||])

rationalReciprocal :: CompiledCode (Rational -> Rational)
rationalReciprocal = $$(compile [||reciprocal||])

rationalPowInteger :: CompiledCode (Rational -> Integer -> Rational)
rationalPowInteger = $$(compile [||powInteger||])

rationalAbs :: CompiledCode (Rational -> Rational)
rationalAbs = $$(compile [||abs||])

rationalProjectAbs :: CompiledCode (Rational -> NatRatio)
rationalProjectAbs = $$(compile [||projectAbs||])

rationalAddExtend :: CompiledCode (NatRatio -> Rational)
rationalAddExtend = $$(compile [||addExtend||])

rationalRestrictMay :: CompiledCode (Rational -> Maybe NatRatio)
rationalRestrictMay = $$(compile [||restrictMay||])

rationalSignum :: CompiledCode (Rational -> Rational)
rationalSignum = $$(compile [||signum||])

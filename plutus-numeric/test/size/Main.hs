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
import PlutusTx.Prelude (BuiltinData)
import PlutusTx.Prelude qualified as PTx
import PlutusTx.TH (compile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Size (fitsOnChain)

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
        ]
    ]

-- Compiled definitions

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

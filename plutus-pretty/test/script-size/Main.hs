{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (
  main,
  testSkeletize,
  testShowSkeletal,
  testTraceSkeletal,
  testTraceErrorSkeletal,
  testTraceIfTrueSkeletal,
  testTraceIfFalseSkeletal,
) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude (BuiltinString)
import PlutusTx.Skeleton (
  Skeleton,
  showSkeletal,
  skeletize,
  traceErrorSkeletal,
  traceIfFalseSkeletal,
  traceIfTrueSkeletal,
  traceSkeletal,
 )
import PlutusTx.TH (compile)
import Test (Foo)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Size (fitsOnChain)

main :: IO ()
main =
  defaultMain . testGroup "On-chain size" $
    [ fitsOnChain "skeletize" . fromCompiledCode $ testSkeletize
    , fitsOnChain "showSkeletal" . fromCompiledCode $ testShowSkeletal
    , fitsOnChain "traceSkeletal" . fromCompiledCode $ testTraceSkeletal
    , fitsOnChain "traceErrorSkeletal" . fromCompiledCode $ testTraceErrorSkeletal
    , fitsOnChain "traceIfFalseSkeletal" . fromCompiledCode $ testTraceIfFalseSkeletal
    , fitsOnChain "traceIfTrueSkeletal" . fromCompiledCode $ testTraceIfTrueSkeletal
    ]

-- Helpers

testSkeletize :: CompiledCode (Foo -> Skeleton)
testSkeletize = $$(compile [||skeletize||])

testShowSkeletal :: CompiledCode (Foo -> BuiltinString)
testShowSkeletal = $$(compile [||showSkeletal||])

testTraceSkeletal :: CompiledCode (Foo -> BuiltinString -> BuiltinString)
testTraceSkeletal = $$(compile [||traceSkeletal||])

testTraceErrorSkeletal :: CompiledCode (Foo -> BuiltinString)
testTraceErrorSkeletal = $$(compile [||traceErrorSkeletal||])

testTraceIfFalseSkeletal :: CompiledCode (Foo -> Bool -> Bool)
testTraceIfFalseSkeletal = $$(compile [||traceIfFalseSkeletal||])

testTraceIfTrueSkeletal :: CompiledCode (Foo -> Bool -> Bool)
testTraceIfTrueSkeletal = $$(compile [||traceIfTrueSkeletal||])

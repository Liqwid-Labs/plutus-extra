{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Test.Tasty.Plutus.Size (fitsInto, bytes)

main :: IO ()
main =
  defaultMain . testGroup "On-chain size" $
    [ fitsInto "skeletize" [bytes| 499 |] . fromCompiledCode $ testSkeletize
    , fitsInto "showSkeletal" [bytes| 2468 |] . fromCompiledCode $ testShowSkeletal
    , fitsInto "traceSkeletal" [bytes| 2470 |] . fromCompiledCode $ testTraceSkeletal
    , fitsInto "traceErrorSkeletal" [bytes| 2484 |] . fromCompiledCode $ testTraceErrorSkeletal
    , fitsInto "traceIfFalseSkeletal" [bytes| 2480 |] . fromCompiledCode $ testTraceIfFalseSkeletal
    , fitsInto "traceIfTrueSkeletal" [bytes| 2480 |] . fromCompiledCode $ testTraceIfTrueSkeletal
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

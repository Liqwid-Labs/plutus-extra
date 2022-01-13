{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}

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
import Test.Tasty.Plutus.Size (bytes, fitsInto)

main :: IO ()
main =
  defaultMain . testGroup "On-chain size" $
    [ fitsInto "skeletize" [bytes| 948 |] . fromCompiledCode $ testSkeletize
    , fitsInto "showSkeletal" [bytes| 2235 |] . fromCompiledCode $ testShowSkeletal
    , fitsInto "traceSkeletal" [bytes| 2238 |] . fromCompiledCode $ testTraceSkeletal
    , fitsInto "traceErrorSkeletal" [bytes| 2253 |] . fromCompiledCode $ testTraceErrorSkeletal
    , fitsInto "traceIfFalseSkeletal" [bytes| 2249 |] . fromCompiledCode $ testTraceIfFalseSkeletal
    , fitsInto "traceIfTrueSkeletal" [bytes| 2249 |] . fromCompiledCode $ testTraceIfTrueSkeletal
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

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

import Cardano.Api.Shelley (
  PlutusScript (PlutusScriptSerialised),
  PlutusScriptV1,
  serialiseToCBOR,
 )
import Codec.Serialise (serialise)
import Data.ByteString qualified as BSS
import Data.ByteString.Lazy qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Plutus.V1.Ledger.Scripts (Script, fromCompiledCode)
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

main :: IO ()
main =
  traverse_
    putStrLn
    [ sizeWithName "skeletize" testSkeletize
    , sizeWithName "showSkeletal" testShowSkeletal
    , sizeWithName "traceSkeletal" testTraceSkeletal
    , sizeWithName "traceErrorSkeletal" testTraceErrorSkeletal
    , sizeWithName "traceIfFalseSkeletal" testTraceIfFalseSkeletal
    , sizeWithName "traceIfTrueSkeletal" testTraceIfTrueSkeletal
    ]

sizeWithName ::
  forall (a :: Type).
  String ->
  CompiledCode a ->
  String
sizeWithName name cc = name <> ": " <> sizeOfCode
  where
    sizeOfCode :: String
    sizeOfCode = produceSize . scriptSize . fromCompiledCode $ cc
    produceSize :: Int -> String
    produceSize i = case i `quotRem` 1024 of
      (d, 0) -> show d <> "KiB"
      (d, r) ->
        show i <> "B (~"
          <> ( if
                  | r <= 256 -> show d <> "KiB)"
                  | r > 256 && r < 768 -> show d <> ".5KiB)"
                  | otherwise -> show (d + 1) <> "KiB)"
             )

scriptSize :: Script -> Int
scriptSize =
  BSS.length
    . serialiseToCBOR
    . PlutusScriptSerialised @PlutusScriptV1
    . SBS.toShort
    . BS.toStrict
    . serialise

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

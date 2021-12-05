{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as PTx
import PlutusTx.TH (compile)
import Test (Foo)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Size (bytes, fitsInto)

main :: IO ()
main =
  defaultMain . testGroup "Size" $
    [ fitsInto "==" [bytes| 241 |] . fromCompiledCode $ fooEq
    , fitsInto "/=" [bytes| 254 |] . fromCompiledCode $ fooNeq
    ]

-- Helpers

fooEq :: CompiledCode (Foo Integer -> Foo Integer -> Bool)
fooEq = $$(compile [||(PTx.==)||])

fooNeq :: CompiledCode (Foo Integer -> Foo Integer -> Bool)
fooNeq = $$(compile [||(PTx./=)||])

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as PTx
import PlutusTx.TH (compile)
import Test (Foo)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Size (fitsOnChain)

main :: IO ()
main =
  defaultMain . testGroup "Size" $
    [ fitsOnChain "==" . fromCompiledCode $ fooEq
    , fitsOnChain "/=" . fromCompiledCode $ fooNeq
    ]

-- Helpers

fooEq :: CompiledCode (Foo Integer -> Foo Integer -> Bool)
fooEq = $$(compile [||(PTx.==)||])

fooNeq :: CompiledCode (Foo Integer -> Foo Integer -> Bool)
fooNeq = $$(compile [||(PTx./=)||])

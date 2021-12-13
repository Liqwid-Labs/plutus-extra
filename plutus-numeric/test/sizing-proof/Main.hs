{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import PlutusTx.Code (CompiledCode)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Size (fitsOnChain)
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.TH (compile)
import Functions (round1, round2, round3, round4)

main :: IO ()
main = defaultMain . testGroup "Demo" $ [
  fitsOnChain "round" . fromCompiledCode $ roundCode,
  fitsOnChain "round1" . fromCompiledCode $ round1Code,
  fitsOnChain "round2" . fromCompiledCode $ round2Code,
  fitsOnChain "round3" . fromCompiledCode $ round3Code,
  fitsOnChain "round4" . fromCompiledCode $ round4Code
  ]

-- Helpers

roundCode :: CompiledCode (Plutus.Rational -> Plutus.Integer)
roundCode = $$(compile [|| Plutus.round ||])

round1Code :: CompiledCode ([Plutus.Rational] -> [Plutus.Integer])
round1Code = $$(compile [|| round1 ||])

round2Code :: CompiledCode ([Plutus.Rational] -> [Plutus.Integer])
round2Code = $$(compile [|| round2 ||])

round3Code :: CompiledCode ([Plutus.Rational] -> [Plutus.Integer])
round3Code = $$(compile [|| round3 ||])

round4Code :: CompiledCode ([Plutus.Rational] -> [Plutus.Integer])
round4Code = $$(compile [|| round4 ||])

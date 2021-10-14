module Main (main) where

import Suites.Natural qualified as Natural
import Suites.NatRatio qualified as NatRatio
import Suites.Numeric qualified as Numeric
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain . testGroup "Tests" $ [
  testGroup "Natural" Natural.tests,
  testGroup "NatRatio" NatRatio.tests,
  testGroup "Numeric.Extra instances for Plutus builtins" Numeric.tests
  ]

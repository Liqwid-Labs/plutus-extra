module Main (main) where

import Suites.Consistency qualified as Consistency
import Suites.NatRatio qualified as NatRatio
import Suites.Natural qualified as Natural
import Suites.Rational qualified as Rational
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain . testGroup "Tests" $
    [ testGroup "Natural" Natural.tests
    , testGroup "Rational" Rational.tests
    , testGroup "NatRatio" NatRatio.tests
    , testGroup "Consistency" Consistency.tests
    ]

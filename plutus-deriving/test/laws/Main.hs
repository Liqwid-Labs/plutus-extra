module Main (main) where

import Test (Foo)
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Plutus.Laws (plutusEqLaws, plutusEqLawsSubstitution)
import Test.Tasty.QuickCheck (QuickCheckTests)

main :: IO ()
main =
  defaultMain . adjustOption (max testMinimum) . testGroup "Laws" $
    [ plutusEqLaws @(Foo Integer)
    , plutusEqLawsSubstitution @(Foo Integer)
    ]
  where
    testMinimum :: QuickCheckTests
    testMinimum = 10_000

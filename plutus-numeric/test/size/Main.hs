module Main (main) where

import Integer qualified
import NatRatio qualified
import Natural qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain . testGroup "Size checks" $
    [ testGroup "Natural" Natural.tests
    , testGroup "Integer" Integer.tests
    , testGroup "NatRatio" NatRatio.tests
    ]

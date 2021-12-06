-- {-# OPTIONS_GHC -Wno-all #-}

module Main (main) where

import Properties.SimpleValidator qualified as SimpleValidator
import Test.Tasty (defaultMain, testGroup)
import Prelude

main :: IO ()
main =
  defaultMain . testGroup "Script properties testing" $
    [ SimpleValidator.tests
    ]

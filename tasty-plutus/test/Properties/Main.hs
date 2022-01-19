{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

--------------------------------------------------------------------------------

import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Plutus.Options (maxSize, testCount)
import Prelude

import MintingPolicy qualified as MP (tests)
import Validator qualified as V (tests)

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    localOption [maxSize| 20 |] $
      localOption [testCount| 100 |] $
        testGroup
          "Property based testing"
          [ V.tests
          , MP.tests
          ]

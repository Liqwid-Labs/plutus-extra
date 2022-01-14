{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Tasty.Plutus.WithScript
 Copyright: (C) MLabs 2022
 License: Apache 2.0
 Maintainer: Sergey Kurgas <sergey@mlabs.city>
 Portability: GHC only
 Stability: Experimental
-}
module Test.Tasty.Plutus.TestScript (
  -- * Wrapper for scripts
  TestScript,

  -- * Helper functions
  mkTestMintingPolicy,
  mkTestMintingPolicyUnsafe,
  mkTestValidator,
  mkTestValidatorUnsafe,
  toTestMintingPolicy,
  toTestValidator,
) where

import Test.Tasty.Plutus.Internal.TestScript (
  TestScript,
  mkTestMintingPolicy,
  mkTestMintingPolicyUnsafe,
  mkTestValidator,
  mkTestValidatorUnsafe,
  toTestMintingPolicy,
  toTestValidator,
 )

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MintingPolicy (tests) where

--------------------------------------------------------------------------------

import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (($), (&&), (*), (+), (==))

import Ledger.Crypto (PubKeyHash)
import Test.QuickCheck.Plutus.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Context (
  ContextBuilder,
  Purpose (ForMinting),
  paysTokensToPubKey,
  spendsTokensFromPubKeySigned,
 )
import Test.Tasty.Plutus.Script.Property (scriptProperty, scriptPropertyPass)
import Test.Tasty.Plutus.TestData (
  Generator (GenForMinting),
  Methodology (Methodology),
  MintingPolicyTask,
  Outcome,
  TestItems (
    ItemsForMinting,
    mpCB,
    mpOutcome,
    mpRedeemer,
    mpTasks
  ),
  Tokens,
  burnTokens,
  mintTokens,
  passIf,
 )
import Test.Tasty.Plutus.TestScript (
  TestScript,
  mkTestMintingPolicy,
  toTestMintingPolicy,
 )
import Test.Tasty.Plutus.WithScript (withMintingPolicy)
import Test.Tasty.QuickCheck (
  Gen,
  arbitrary,
  elements,
  genericShrink,
 )

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx.Prelude (traceIfFalse, ($), (==))
import PlutusTx.TH (compile)
import Wallet.Emulator.Types (WalletNumber (WalletNumber))
import Wallet.Emulator.Wallet (fromWalletNumber, walletPubKeyHash)

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  withMintingPolicy "myMintinPolicy:" myMintingPolicyScript $ do
    scriptProperty "MintingPolicy checks the secret" $
      GenForMinting gen1 transform1
    scriptPropertyPass "MintingPolicy always succeeds if the key is correct" $
      GenForMinting gen1 transform2

gen1 :: Methodology (Integer, Tokens, Tokens)
gen1 = Methodology gen' genericShrink
  where
    gen' :: Gen (Integer, Tokens, Tokens)
    gen' = do
      randomKey <- arbitrary
      key <- elements [randomKey, secretKey]
      toksMint <- arbitrary
      toksBurn <- arbitrary
      pure (key, toksMint, toksBurn)

-- | Creates TestItems with an arbitrary key used in Redeemer
transform1 ::
  (Integer, Tokens, Tokens) ->
  TestItems ( 'ForMinting Integer)
transform1 (key, toksMint, toksBurn) =
  ItemsForMinting
    { mpRedeemer = key
    , mpTasks = tasks
    , mpCB = cb
    , mpOutcome = out
    }
  where
    tasks :: NonEmpty MintingPolicyTask
    tasks =
      mintTokens toksMint
        <> burnTokens toksBurn
    out :: Outcome
    out = passIf $ key == secretKey
    cb :: ContextBuilder ( 'ForMinting Integer)
    cb =
      paysTokensToPubKey userPKHash1 toksMint
        <> spendsTokensFromPubKeySigned userPKHash2 toksBurn

-- | Creates TestItems with correct secretKey used in Redeemer
transform2 ::
  (Integer, Tokens, Tokens) ->
  TestItems ( 'ForMinting Integer)
transform2 = transformItems . transform1
  where
    transformItems ::
      TestItems ( 'ForMinting Integer) ->
      TestItems ( 'ForMinting Integer)
    transformItems = \case
      ItemsForMinting _ t c o -> ItemsForMinting secretKey t c o

{- | A MintingPolicy for testing property-based testing functionality.

  MintingPolicy logic:

  MintingPolicy accepts a secret as a parameter.
  In order to mint anything, you need to provide this secret as a Redeemer.
-}
myMintingPolicy :: Integer -> Integer -> ScriptContext -> Bool
myMintingPolicy secret key _ = correctKey
  where
    correctKey :: Bool
    correctKey =
      traceIfFalse "The provided key is wrong" $
        secret == key

myMintingPolicyScript :: TestScript ( 'ForMinting Integer)
myMintingPolicyScript =
  mkTestMintingPolicy
    $$(compile [||myMintingPolicy secretKey||])
    $$(compile [||toTestMintingPolicy||])

userPKHash1 :: PubKeyHash
userPKHash1 = walletPubKeyHash $ fromWalletNumber $ WalletNumber 1

userPKHash2 :: PubKeyHash
userPKHash2 = walletPubKeyHash $ fromWalletNumber $ WalletNumber 2

{-# INLINEABLE secretKey #-}
secretKey :: Integer
secretKey = 1128

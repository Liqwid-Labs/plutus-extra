{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module MintingPolicy (tests) where

--------------------------------------------------------------------------------

import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (($), (&&), (*), (+), (==))

import Ledger.Crypto (PubKeyHash)
import Ledger.Typed.Scripts (MintingPolicy)
import Plutus.V1.Ledger.Scripts (mkMintingPolicyScript)
import Plutus.V1.Ledger.Value (TokenName)
import PlutusTx.Positive (Positive)
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
  MintingPolicyQuery,
  Outcome,
  TestItems (
    ItemsForMinting,
    mintCB,
    mintOutcome,
    mintRedeemer,
    mintTokens
  ),
  burningTokens,
  mintingTokens,
  passIf,
 )
import Test.Tasty.Plutus.WithScript (toTestMintingPolicy, withMintingPolicy)
import Test.Tasty.QuickCheck (
  Gen,
  arbitrary,
  elements,
  genericShrink,
 )

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx (applyCode)
import PlutusTx.Builtins (BuiltinData)
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

gen1 :: Methodology (Integer, (TokenName, Positive), (TokenName, Positive))
gen1 = Methodology gen' genericShrink
  where
    gen' :: Gen (Integer, (TokenName, Positive), (TokenName, Positive))
    gen' = do
      randomKey <- arbitrary
      key <- elements [randomKey, secretKey]
      toksMint <- arbitrary
      toksBurn <- arbitrary
      pure (key, toksMint, toksBurn)

-- | Creates TestItems with an arbitrary key used in Redeemer
transform1 ::
  (Integer, (TokenName, Positive), (TokenName, Positive)) ->
  TestItems ( 'ForMinting Integer)
transform1 (key, (tnMint, posMint), (tnBurn, posBurn)) =
  ItemsForMinting
    { mintRedeemer = key
    , mintTokens = toks
    , mintCB = cb
    , mintOutcome = out
    }
  where
    toks :: NonEmpty MintingPolicyQuery
    toks =
      mintingTokens tnMint posMint
        <> burningTokens tnBurn posBurn
    out :: Outcome
    out = passIf $ key == secretKey
    cb :: ContextBuilder ( 'ForMinting Integer)
    cb =
      paysTokensToPubKey userPKHash1 (tnMint, posMint)
        <> spendsTokensFromPubKeySigned userPKHash2 (tnBurn, posBurn)

-- | Creates TestItems with correct secretKey used in Redeemer
transform2 ::
  (Integer, (TokenName, Positive), (TokenName, Positive)) ->
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

myMintingPolicyScript :: MintingPolicy
myMintingPolicyScript =
  mkMintingPolicyScript $
    $$(compile [||wrap||])
      `applyCode` $$(compile [||myMintingPolicy secretKey||])
  where
    wrap ::
      (Integer -> ScriptContext -> Bool) ->
      (BuiltinData -> BuiltinData -> ())
    wrap = toTestMintingPolicy

userPKHash1 :: PubKeyHash
userPKHash1 = walletPubKeyHash $ fromWalletNumber $ WalletNumber 1

userPKHash2 :: PubKeyHash
userPKHash2 = walletPubKeyHash $ fromWalletNumber $ WalletNumber 2

{-# INLINEABLE secretKey #-}
secretKey :: Integer
secretKey = 1128

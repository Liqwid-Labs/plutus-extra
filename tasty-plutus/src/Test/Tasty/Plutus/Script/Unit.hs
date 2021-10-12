{-# LANGUAGE Trustworthy #-}

{- |
 Module: Test.Tasty.Plutus.Script.Unit
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A unit-test-like interface for validator and minting policy testing.

 = Example usage

 > myTests :: TestTree
 > myTests = withValidator "Testing my spending" myValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    ...

 = Note

 This re-exports multiple definitions for backwards-compatibility reasons.
 Many of these will disappear on the next major version bump: the only definitions
 that are guaranteed to remain are:

 * 'shouldValidate'
 * 'shouldn'tValidate'
-}
module Test.Tasty.Plutus.Script.Unit (
  -- * Validator context types
  TestData (..),
  WithScript,

  -- * Wrappers
  toTestValidator,
  toTestMintingPolicy,

  -- * Testing API
  withValidator,
  withMintingPolicy,
  shouldValidate,
  shouldn'tValidate,

  -- * Options
  Fee (..),
  TimeRange (..),
  TestTxId (..),
  TestCurrencySymbol (..),
  TestValidatorHash (..),
) where

import Control.Monad.Reader (asks)
import Control.Monad.Writer (tell)
import Data.Proxy (Proxy (Proxy))
import Data.Sequence qualified as Seq
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger.Value (
  CurrencySymbol (unCurrencySymbol),
  TokenName (unTokenName),
  Value (getValue),
 )
import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Contexts (
  ScriptContext (
    scriptContextPurpose,
    scriptContextTxInfo
  ),
  ScriptPurpose (Certifying, Minting, Rewarding, Spending),
  TxInInfo (
    txInInfoOutRef,
    txInInfoResolved
  ),
  TxInfo (
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOut (
    txOutAddress,
    txOutDatumHash,
    txOutValue
  ),
  TxOutRef (
    txOutRefId,
    txOutRefIdx
  ),
 )
import Plutus.V1.Ledger.Credential (
  Credential (
    PubKeyCredential,
    ScriptCredential
  ),
  StakingCredential (
    StakingHash,
    StakingPtr
  ),
 )
import Plutus.V1.Ledger.Crypto (PubKeyHash (getPubKeyHash))
import Plutus.V1.Ledger.DCert (
  DCert (
    DCertDelegDeRegKey,
    DCertDelegDelegate,
    DCertDelegRegKey,
    DCertGenesis,
    DCertMir,
    DCertPoolRegister,
    DCertPoolRetire
  ),
 )
import Plutus.V1.Ledger.Interval (
  Closure,
  Extended,
  Interval,
  LowerBound,
  UpperBound,
 )
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (
  Context (Context),
  Datum (Datum, getDatum),
  DatumHash (DatumHash),
  MintingPolicy,
  Redeemer (Redeemer),
  ScriptError,
  Validator,
  ValidatorHash (ValidatorHash),
  runMintingPolicyScript,
  runScript,
 )
import Plutus.V1.Ledger.Time (POSIXTime (getPOSIXTime))
import Plutus.V1.Ledger.TxId (TxId (getTxId))
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.IsData.Class (
  ToData (toBuiltinData),
 )
import Safe (lastMay)
import Test.Tasty.Options (
  OptionDescription (Option),
  lookupOption,
 )
import Test.Tasty.Plutus.Internal (
  ContextBuilder,
  Purpose (ForMinting, ForSpending),
  TransactionConfig (
    TransactionConfig,
    testCurrencySymbol,
    testFee,
    testTimeRange,
    testTxId,
    testValidatorHash
  ),
  WithScript (WithMinting, WithSpending),
  compileMinting,
  compileSpending,
  ourStyle,
 )
import Test.Tasty.Plutus.Options (
  Fee (Fee),
  TestCurrencySymbol (TestCurrencySymbol),
  TestTxId (TestTxId),
  TestValidatorHash (TestValidatorHash),
  TimeRange (TimeRange),
 )
import Test.Tasty.Plutus.TestData (TestData (MintingTest, SpendingTest))
import Test.Tasty.Plutus.WithScript (
  toTestMintingPolicy,
  toTestValidator,
  withMintingPolicy,
  withValidator,
 )
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  Result,
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (
  Doc,
  colon,
  hang,
  int,
  renderStyle,
  text,
  vcat,
  ($+$),
  (<+>),
 )
import Text.Show.Pretty (ppDoc, valToDoc)
import Text.Show.Pretty qualified as Pretty
import Type.Reflection (Typeable)

{- | Specify that, given this test data and context, the validation should
 succeed.

 @since 3.0
-}
shouldValidate ::
  forall (p :: Purpose).
  (Typeable p) =>
  String ->
  TestData p ->
  ContextBuilder p ->
  WithScript p ()
shouldValidate name td cb = case td of
  SpendingTest {} -> WithSpending $ do
    tt <- asks (singleTest name . Spender Pass td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter Pass td cb)
    tell . Seq.singleton $ tt

{- | Specify that, given this test data and context, the validation should fail.

 @since 3.0
-}
shouldn'tValidate ::
  forall (p :: Purpose).
  (Typeable p) =>
  String ->
  TestData p ->
  ContextBuilder p ->
  WithScript p ()
shouldn'tValidate name td cb = case td of
  SpendingTest {} -> WithSpending $ do
    tt <- asks (singleTest name . Spender Fail td cb)
    tell . Seq.singleton $ tt
  MintingTest {} -> WithMinting $ do
    tt <- asks (singleTest name . Minter Fail td cb)
    tell . Seq.singleton $ tt

-- Helpers

data Outcome = Fail | Pass

data ValidatorTest (p :: Purpose) where
  Spender ::
    Outcome ->
    TestData 'ForSpending ->
    ContextBuilder 'ForSpending ->
    Validator ->
    ValidatorTest 'ForSpending
  Minter ::
    Outcome ->
    TestData 'ForMinting ->
    ContextBuilder 'ForMinting ->
    MintingPolicy ->
    ValidatorTest 'ForMinting

instance (Typeable p) => IsTest (ValidatorTest p) where
  run opts vt _ = pure $ case vt of
    Spender expected td@(SpendingTest d r v) cb val ->
      let context = compileSpending conf cb d v
          context' = Context . toBuiltinData $ context
          d' = Datum . toBuiltinData $ d
          r' = Redeemer . toBuiltinData $ r
       in case runScript context' val d' r' of
            Left err -> testFailed . formatScriptError $ err
            Right (_, logs) -> deliverResult expected logs conf context td
    Minter expected td@(MintingTest r) cb mp ->
      let context = compileMinting conf cb
          context' = Context . toBuiltinData $ context
          r' = Redeemer . toBuiltinData $ r
       in case runMintingPolicyScript context' mp r' of
            Left err -> testFailed . formatScriptError $ err
            Right (_, logs) -> deliverResult expected logs conf context td
    where
      conf :: TransactionConfig
      conf =
        TransactionConfig
          { testFee = testFee'
          , testTimeRange = testTimeRange'
          , testTxId = testTxId'
          , testCurrencySymbol = testCurrencySymbol'
          , testValidatorHash = testValidatorHash'
          }
      testFee' :: Value
      Fee testFee' = lookupOption opts
      testTimeRange' :: Interval POSIXTime
      TimeRange testTimeRange' = lookupOption opts
      testTxId' :: TxId
      TestTxId testTxId' = lookupOption opts
      testCurrencySymbol' :: CurrencySymbol
      TestCurrencySymbol testCurrencySymbol' = lookupOption opts
      testValidatorHash' :: ValidatorHash
      TestValidatorHash testValidatorHash' = lookupOption opts
  testOptions =
    Tagged
      [ Option @Fee Proxy
      , Option @TimeRange Proxy
      , Option @TestTxId Proxy
      , Option @TestCurrencySymbol Proxy
      , Option @TestValidatorHash Proxy
      ]

deliverResult ::
  forall (p :: Purpose).
  Outcome ->
  [Text] ->
  TransactionConfig ->
  ScriptContext ->
  TestData p ->
  Result
deliverResult expected logs conf sc td =
  case (expected, lastMay logs >>= Text.stripPrefix "tasty-plutus: ") of
    (_, Nothing) -> testFailed noOutcome
    (Fail, Just "Pass") -> testFailed unexpectedSuccess
    (Fail, Just "Fail") -> testPassed ""
    (Pass, Just "Pass") -> testPassed ""
    (Pass, Just "Fail") -> testFailed unexpectedFailure
    (_, Just t) -> case Text.stripPrefix "Parse failed: " t of
      Nothing -> testFailed . internalError $ t
      Just t' -> testFailed . noParse $ t'
  where
    noOutcome :: String
    noOutcome =
      renderStyle ourStyle $
        "No outcome from run"
          $+$ dumpState
          $+$ ""
          $+$ "Did you forget to use toTestValidator or toTestMintingPolicy?"
    unexpectedSuccess :: String
    unexpectedSuccess =
      renderStyle ourStyle $
        "Unexpected success" $+$ dumpState
    unexpectedFailure :: String
    unexpectedFailure =
      renderStyle ourStyle $
        "Unexpected failure" $+$ dumpState
    internalError :: Text -> String
    internalError msg =
      renderStyle ourStyle $
        ("Internal error" <+> (text . show $ msg)) $+$ dumpState
    noParse :: Text -> String
    noParse what =
      renderStyle ourStyle $
        ((text . show $ what) <+> "did not parse") $+$ dumpState
    dumpState :: Doc
    dumpState =
      ""
        $+$ hang "Context" 4 (scToDoc sc)
        $+$ hang "Configuration" 4 (ppDoc conf)
        $+$ hang "Inputs" 4 dumpInputs
        $+$ hang "Logs" 4 dumpLogs
    dumpInputs :: Doc
    dumpInputs = case td of
      SpendingTest d r v ->
        "Datum"
          $+$ ppDoc d
          $+$ "Redeemer"
          $+$ ppDoc r
          $+$ "Value"
          $+$ ppDoc v
      MintingTest r ->
        "Redeemer" $+$ ppDoc r
    dumpLogs :: Doc
    dumpLogs = vcat . fmap go . zip [1 ..] $ logs
    go :: (Int, Text) -> Doc
    go (ix, line) = (int ix <> colon) <+> (text . show $ line)

formatScriptError :: ScriptError -> String
formatScriptError =
  renderStyle ourStyle . hang "Script execution error:" 4 . ppDoc

-- ScriptContext unparsing breaks due to a non-standard Show instance somewhere
-- in its guts. This works around that by building it up manually. - Koz
scToDoc :: ScriptContext -> Doc
scToDoc = valToDoc . go
  where
    go :: ScriptContext -> Pretty.Value
    go sc =
      let scTxInfo = scriptContextTxInfo sc
          scPurpose = scriptContextPurpose sc
       in Pretty.Rec
            "ScriptContext"
            [ ("scriptContextTxInfo", txInfoToVal scTxInfo)
            , ("scriptContextTxInfo", purposeToVal scPurpose)
            ]

txInfoToVal :: TxInfo -> Pretty.Value
txInfoToVal txi =
  Pretty.Rec
    "TxInfo"
    [ ("txInfoInputs", Pretty.List . fmap txInInfoToVal . txInfoInputs $ txi)
    , ("txInfoOutputs", Pretty.List . fmap txOutToVal . txInfoOutputs $ txi)
    , ("txInfoFee", valueToVal . txInfoFee $ txi)
    , ("txInfoMint", valueToVal . txInfoMint $ txi)
    , ("txInfoDCert", Pretty.List . fmap dcertToVal . txInfoDCert $ txi)
    , ("txInfoWdrl", Pretty.List . fmap sciToVal . txInfoWdrl $ txi)
    , ("txInfoValidRange", timeRangeToVal . txInfoValidRange $ txi)
    , ("txInfoSignatories", Pretty.List . fmap pkhToVal . txInfoSignatories $ txi)
    , ("txInfoData", Pretty.List . fmap dhdToVal . txInfoData $ txi)
    , ("txInfoId", txIdToVal . txInfoId $ txi)
    ]

purposeToVal :: ScriptPurpose -> Pretty.Value
purposeToVal = \case
  Minting cs -> Pretty.Con "Minting" [currencySymbolToVal cs]
  Spending txOutRef -> Pretty.Con "Spending" [txOutRefToVal txOutRef]
  Rewarding sc -> Pretty.Con "Rewarding" [scToVal sc]
  Certifying dc -> Pretty.Con "Certifying" [dcertToVal dc]

txInInfoToVal :: TxInInfo -> Pretty.Value
txInInfoToVal tii =
  Pretty.Rec
    "TxInInfo"
    [ ("txInInfoOutRef", txOutRefToVal . txInInfoOutRef $ tii)
    , ("txInInfoResolved", txOutToVal . txInInfoResolved $ tii)
    ]

txOutToVal :: TxOut -> Pretty.Value
txOutToVal txo =
  Pretty.Rec
    "TxOut"
    [ ("txOutAddress", addressToVal . txOutAddress $ txo)
    , ("txOutValue", valueToVal . txOutValue $ txo)
    ,
      ( "txOutDatumHash"
      , maybe (Pretty.Con "Nothing" []) go
          . txOutDatumHash
          $ txo
      )
    ]
  where
    go :: DatumHash -> Pretty.Value
    go dh = Pretty.Con "Just" [dhToVal dh]

valueToVal :: Value -> Pretty.Value
valueToVal val = Pretty.Con "Value" [mapToVal . getValue $ val]

dcertToVal :: DCert -> Pretty.Value
dcertToVal = \case
  DCertDelegRegKey sc -> Pretty.Con "DCertDelegRegKey" [scToVal sc]
  DCertDelegDeRegKey sc -> Pretty.Con "DCertDelegDeRegKey" [scToVal sc]
  DCertDelegDelegate sc pkh ->
    Pretty.Con "DCertDelegDelegate" [scToVal sc, pkhToVal pkh]
  DCertPoolRegister poolId poolVFR ->
    Pretty.Con "DCertPoolRegister" [pkhToVal poolId, pkhToVal poolVFR]
  DCertPoolRetire pkh i ->
    Pretty.Con "DCertPoolRetire" [pkhToVal pkh, integerToVal i]
  DCertGenesis -> Pretty.Con "DCertGenesis" []
  DCertMir -> Pretty.Con "DCertMir" []

sciToVal :: (StakingCredential, Integer) -> Pretty.Value
sciToVal (sc, i) = Pretty.Tuple [scToVal sc, integerToVal i]

timeRangeToVal :: Interval POSIXTime -> Pretty.Value
timeRangeToVal inter =
  Pretty.Rec
    "Interval"
    [ ("ivFrom", lbToVal . Interval.ivFrom $ inter)
    , ("ivTo", ubToVal . Interval.ivTo $ inter)
    ]

pkhToVal :: PubKeyHash -> Pretty.Value
pkhToVal pkh =
  Pretty.Con "PubKeyHash" [Pretty.String . show . getPubKeyHash $ pkh]

dhdToVal :: (DatumHash, Datum) -> Pretty.Value
dhdToVal (dh, d) = Pretty.Tuple [dhToVal dh, datumToVal d]

txIdToVal :: TxId -> Pretty.Value
txIdToVal ti =
  Pretty.Con "TxId" [Pretty.String . show . getTxId $ ti]

currencySymbolToVal :: CurrencySymbol -> Pretty.Value
currencySymbolToVal cs =
  Pretty.Con "CurrencySymbol" [Pretty.String . show . unCurrencySymbol $ cs]

txOutRefToVal :: TxOutRef -> Pretty.Value
txOutRefToVal tor =
  Pretty.Rec
    "TxOutRef"
    [ ("txOutRefId", txIdToVal . txOutRefId $ tor)
    , ("txOutRefIdx", integerToVal . txOutRefIdx $ tor)
    ]

scToVal :: StakingCredential -> Pretty.Value
scToVal = \case
  StakingHash cred -> Pretty.Con "StakingHash" [credToVal cred]
  StakingPtr i1 i2 i3 ->
    Pretty.Con "StakingPtr" [integerToVal i1, integerToVal i2, integerToVal i3]

addressToVal :: Address -> Pretty.Value
addressToVal _ = Pretty.Con "Address" []

dhToVal :: DatumHash -> Pretty.Value
dhToVal (DatumHash dh) =
  Pretty.Con "DatumHash" [Pretty.String . show $ dh]

mapToVal ::
  AssocMap.Map CurrencySymbol (AssocMap.Map TokenName Integer) ->
  Pretty.Value
mapToVal m =
  Pretty.Con "AssocMap" [Pretty.List . fmap go . AssocMap.toList $ m]
  where
    go :: (CurrencySymbol, AssocMap.Map TokenName Integer) -> Pretty.Value
    go (cs, m') =
      Pretty.Tuple
        [ currencySymbolToVal cs
        , Pretty.Con "AssocMap" [Pretty.List . fmap go2 . AssocMap.toList $ m']
        ]
    go2 :: (TokenName, Integer) -> Pretty.Value
    go2 (tn, i) = Pretty.Tuple [tokenNameToVal tn, integerToVal i]

integerToVal :: Integer -> Pretty.Value
integerToVal i = case signum i of
  (-1) -> Pretty.Neg . Pretty.Integer . show . abs $ i
  _ -> Pretty.Integer . show . abs $ i

lbToVal :: LowerBound POSIXTime -> Pretty.Value
lbToVal (Interval.LowerBound ext clos) =
  Pretty.Con "LowerBound" [extToVal ext, closToVal clos]

ubToVal :: UpperBound POSIXTime -> Pretty.Value
ubToVal (Interval.UpperBound ext clos) =
  Pretty.Con "UpperBound" [extToVal ext, closToVal clos]

datumToVal :: Datum -> Pretty.Value
datumToVal d =
  Pretty.Con "Datum" [Pretty.String . show . getDatum $ d]

credToVal :: Credential -> Pretty.Value
credToVal = \case
  PubKeyCredential pkh ->
    Pretty.Con "PubKeyCredential" [pkhToVal pkh]
  ScriptCredential vh ->
    Pretty.Con "ScriptCredential" [vhToVal vh]

tokenNameToVal :: TokenName -> Pretty.Value
tokenNameToVal tn =
  Pretty.Con "TokenName" [Pretty.String . show . unTokenName $ tn]

extToVal :: Extended POSIXTime -> Pretty.Value
extToVal = \case
  Interval.NegInf -> Pretty.Con "NegInf" []
  Interval.Finite t -> Pretty.Con "Finite" [timeToVal t]
  Interval.PosInf -> Pretty.Con "PosInf" []

closToVal :: Closure -> Pretty.Value
closToVal = Pretty.String . show

vhToVal :: ValidatorHash -> Pretty.Value
vhToVal (ValidatorHash vh) =
  Pretty.Con "ValidatorHash" [Pretty.String . show $ vh]

timeToVal :: POSIXTime -> Pretty.Value
timeToVal t =
  Pretty.Con "POSIXTime" [integerToVal . getPOSIXTime $ t]

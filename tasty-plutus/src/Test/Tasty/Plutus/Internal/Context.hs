module Test.Tasty.Plutus.Internal.Context (
  Purpose (..),
  ExternalType (..),
  ValueType (..),
  Input (..),
  Output (..),
  Minting (..),
  TransactionConfig (..),
  ContextBuilder (..),
  compileSpending,
  compileMinting,
  makeIncompleteContexts,
  outputsToInputs,
) where

import Control.Arrow ((***))
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup (sconcat)
import Data.Sequence (Seq)
import GHC.Exts (toList)
import Ledger.Scripts (datumHash)
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Api (
  BuiltinData,
  CurrencySymbol,
  Datum (Datum),
  DatumHash,
  Interval,
  PubKeyHash,
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting, Spending),
  ToData (toBuiltinData),
  TokenName,
  TxId (TxId),
  TxInInfo (TxInInfo),
  TxInfo (
    TxInfo,
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
  TxOut (TxOut),
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value,
 )
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V1.Ledger.Value.Extra (filterValue)
import PlutusTx.Positive (Positive, getPositive)
import PlutusTx.Prelude (length)
import Test.Tasty.Plutus.Internal.Minting (
  MintingPolicyAction (BurnAction, MintAction),
  MintingPolicyTask (MPTask),
  Tokens (Tokens),
 )
import Test.Tasty.Plutus.Options (ScriptInputPosition (Head, Tail))
import Prelude hiding (length)

{- | Describes what kind of script this is meant to test. Directly
 corresponds to 'ScriptPurpose'.

 @since 1.0
-}
data Purpose where
  -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Minting'. This tag applies
  -- to minting-policy-related structures and functions.
  --
  -- @since 1.0
  ForMinting ::
    forall redeemer.
    -- | @since 5.1
    redeemer ->
    Purpose
  -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Spending'. This tag applies
  -- to validator-related structures and functions.
  --
  -- @since 1.0
  ForSpending ::
    forall datum redeemer.
    -- | @since 5.1
    datum ->
    -- | @since 5.1
    redeemer ->
    Purpose

{- | \'Marker type\' for input and output metadata.

 @since 3.0
-}
data ExternalType
  = -- | @since 5.3
    PubKeyType PubKeyHash (Maybe BuiltinData)
  | -- | @since 3.0
    ScriptType ValidatorHash BuiltinData
  | -- | @since 3.0
    OwnType BuiltinData
  deriving stock
    ( -- | @since 3.0
      Show
    )

{- | Different types of value:

  'TokensValue' is only used within @ContextBuilder ('ForMinting r)@
  for representing classes, belonging to the tested MintingPolicy.

  In all other cases 'GeneralValue' is used.

 @since 6.0
-}
data ValueType
  = -- | @since 6.0
    GeneralValue Value
  | -- | @since 6.0
    TokensValue TokenName Positive
  deriving stock
    ( -- | @since 6.0
      Eq
    , -- | @since 6.0
      Show
    )

{- | An input to a script, consisting of a value and a type.

 @since 1.0
-}
data Input
  = -- | @since 6.0
    Input ExternalType ValueType
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | An output from a script, consisting of a value and a type.

 @since 1.0
-}
data Output
  = -- | @since 6.0
    Output ExternalType ValueType
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | A 'Value' minted with a minting policy other than the one being tested.
 Do not use this for tokens being minted by the tested minting policy.

 = Note

 Asset classes with 'CurrencySymbol' matching 'testCurrencySymbol' in 'TransactionConfig'
 will be excluded from the resulting 'ScriptContext'.

 @since 3.0
-}
data Minting
  = -- | @since 4.1
    Mint Value
  deriving stock
    ( -- | @since 3.0
      Show
    )

data TransactionConfig = TransactionConfig
  { testFee :: Value
  , testTimeRange :: Interval POSIXTime
  , testTxId :: TxId
  , testCurrencySymbol :: CurrencySymbol
  , testValidatorHash :: ValidatorHash
  , scriptInputPosition :: ScriptInputPosition
  }
  deriving stock (Show)

{- | A way to incrementally build up a script context.

 It is tagged with a 'Purpose' as a marker for what kind of script it's
 supposed to be validating.

 You can use the 'Semigroup' instance of this type to build up larger
 contexts. For example:

 > let cb = paysToWallet aWallet someValue <>
 >          signedWith aHash <>
 >          tagged aUniqueTag

 @since 1.0
-}
data ContextBuilder (p :: Purpose) = ContextBuilder
  { -- | @since 7.1
    cbInputs :: Seq Input
  , -- | @since 7.1
    cbOutputs :: Seq Output
  , -- | @since 7.1
    cbSignatories :: Seq PubKeyHash
  , -- | @since 7.1
    cbDatums :: Seq BuiltinData
  , -- | @since 7.1
    cbMinting :: Seq Minting
  }

-- | @since 1.0
deriving stock instance Show (ContextBuilder p)

-- | @since 1.0
instance Semigroup (ContextBuilder p) where
  {-# INLINEABLE (<>) #-}
  ContextBuilder is os pkhs ts ms <> ContextBuilder is' os' pkhs' ts' ms' =
    ContextBuilder (is <> is') (os <> os') (pkhs <> pkhs') (ts <> ts') (ms <> ms')

-- | @since 3.4
instance Monoid (ContextBuilder p) where
  {-# INLINEABLE mempty #-}
  mempty = ContextBuilder mempty mempty mempty mempty mempty

compileSpending ::
  forall (datum :: Type) (redeemer :: Type).
  (ToData datum) =>
  TransactionConfig ->
  ContextBuilder ( 'ForSpending datum redeemer) ->
  datum ->
  Value ->
  ScriptContext
compileSpending conf cb d v =
  ScriptContext go
    . Spending
    . TxOutRef (testTxId conf)
    $ 0
  where
    go :: TxInfo
    go =
      let dt = toBuiltinData d
          baseInfo = baseTxInfo conf cb
          inInfo = createTxInInfo conf (0, Input (OwnType dt) (GeneralValue v))
          inData = datumWithHash dt
       in baseInfo
            { txInfoInputs = case scriptInputPosition conf of
                Head -> inInfo : txInfoInputs baseInfo
                Tail -> txInfoInputs baseInfo <> [inInfo]
            , txInfoData = inData : txInfoData baseInfo
            }

compileMinting ::
  forall (r :: Type).
  TransactionConfig ->
  ContextBuilder ( 'ForMinting r) ->
  NonEmpty MintingPolicyTask ->
  ScriptContext
compileMinting conf cb toks =
  ScriptContext go (Minting sym)
  where
    sym :: CurrencySymbol
    sym = testCurrencySymbol conf

    mintingValue :: Value
    mintingValue =
      Map.foldMapWithKey (Value.singleton sym)
        . Map.fromList
        . map processMPTask
        . NonEmpty.toList
        $ toks

    go :: TxInfo
    go =
      let baseInfo = baseTxInfo conf cb
       in baseInfo
            { txInfoMint =
                mintingValue <> txInfoMint baseInfo
            }
    processMPTask :: MintingPolicyTask -> (TokenName, Integer)
    processMPTask (MPTask action (Tokens tn pos)) =
      let i = case action of
            MintAction -> getPositive pos
            BurnAction -> negate $ getPositive pos
       in (tn, i)

{- | Combine a list of partial contexts that should,
     when combined, validate, but fail when any one
     partial context is missing. The input is a list
     of pairs where the first element is the partial
     context, and the second is the test message when
     that particular context is missing. e.g.

     > makeIncompleteContexts
     >   [ (context1, "Missing context 1")
     >   , (context2, "Missing context 2")
     >   , (context3, "Missing context 3")
     >   ]

     is equivalent to

     > [ (context2 <> context3, "Missing context 1")
     > , (context1 <> context3, "Missing context 2")
     > , (context1 <> context2, "Missing context 3")
     > ]

     This can then be run in a `withTestScript` block
     like so:

     > mapM_ (\(ctx,str) -> shouldn'tValidate str input ctx) convertedContexts

 @since 4.1
-}
makeIncompleteContexts ::
  forall (p :: Purpose).
  [(ContextBuilder p, String)] ->
  [(ContextBuilder p, String)]
makeIncompleteContexts ctxs = map (first sconcat) ctxs2
  where
    ctxs1 = removeContext ctxs
    ctxs2 = mapMaybe nonEmpty1st ctxs1
    nonEmpty1st :: ([a], b) -> Maybe (NonEmpty a, b)
    nonEmpty1st (xs, y) = (,y) <$> NonEmpty.nonEmpty xs

{- All context outputs are converted to a list of inputs:
 these can be used to build new contexts.

 = Example

 @
  foldMap input (outputsToInputs initContext) <> restOfContext
 @

 @since 4.2
-}
outputsToInputs :: ContextBuilder p -> [Input]
outputsToInputs (ContextBuilder _ outs _ _ _) =
  toList $ fmap (\(Output t v) -> Input t v) outs

-- Helpers

baseTxInfo ::
  forall (p :: Purpose).
  TransactionConfig ->
  ContextBuilder p ->
  TxInfo
baseTxInfo conf (ContextBuilder ins outs pkhs dats mints) =
  let valHash = testValidatorHash conf
      value = foldMap (filterValue filterCS . unMint) mints
   in TxInfo
        { txInfoInputs = createTxInInfo conf <$> indexedInputs
        , txInfoOutputs = toList . fmap (toTxOut conf valHash) $ outs
        , txInfoFee = testFee conf
        , txInfoMint = value
        , txInfoDCert = []
        , txInfoWdrl = []
        , txInfoValidRange = testTimeRange conf
        , txInfoSignatories = toList pkhs
        , txInfoData =
            (mapMaybe toInputDatum . toList $ ins)
              <> (mapMaybe toOutputDatum . toList $ outs)
              <> (toList . fmap datumWithHash $ dats)
        , txInfoId = TxId "testTx"
        }
  where
    indexedInputs :: [(Integer, Input)]
    indexedInputs = zip [1 ..] . toList $ ins

    unMint :: Minting -> Value
    unMint (Mint val) = val

    filterCS :: CurrencySymbol -> TokenName -> Integer -> Bool
    filterCS cs _ _ = cs /= testCurrencySymbol conf

toInputDatum :: Input -> Maybe (DatumHash, Datum)
toInputDatum (Input typ _) = case typ of
  ScriptType _ dt -> Just . datumWithHash $ dt
  OwnType dt -> Just . datumWithHash $ dt
  PubKeyType _ dt -> datumWithHash <$> dt

toOutputDatum :: Output -> Maybe (DatumHash, Datum)
toOutputDatum (Output typ _) = case typ of
  ScriptType _ dt -> Just . datumWithHash $ dt
  OwnType dt -> Just . datumWithHash $ dt
  PubKeyType _ dt -> datumWithHash <$> dt

datumWithHash :: BuiltinData -> (DatumHash, Datum)
datumWithHash dt = (datumHash dt', dt')
  where
    dt' :: Datum
    dt' = Datum dt

createTxInInfo :: TransactionConfig -> (Integer, Input) -> TxInInfo
createTxInInfo conf (ix, Input typ valType) =
  let val = extractValue conf valType
   in TxInInfo (TxOutRef (testTxId conf) ix) $
        case typ of
          PubKeyType pkh dat -> TxOut (pubKeyHashAddress pkh) val $ datumHash . Datum <$> dat
          ScriptType hash dat ->
            TxOut (scriptHashAddress hash) val . justDatumHash $ dat
          OwnType dat ->
            TxOut (scriptHashAddress . testValidatorHash $ conf) val . justDatumHash $ dat

justDatumHash :: BuiltinData -> Maybe DatumHash
justDatumHash = Just . datumHash . Datum

toTxOut :: TransactionConfig -> ValidatorHash -> Output -> TxOut
toTxOut conf valHash (Output typ valType) =
  let val = extractValue conf valType
   in case typ of
        PubKeyType pkh dat -> TxOut (pubKeyHashAddress pkh) val $ datumHash . Datum <$> dat
        ScriptType hash dat ->
          TxOut (scriptHashAddress hash) val . justDatumHash $ dat
        OwnType dat ->
          TxOut (scriptHashAddress valHash) val . justDatumHash $ dat

-- Helper for makeIncompleteContexts : Take a list
-- of pairs, and create another list of pairs where
-- the first element is the concatenation of every
-- element but the n-th, and the second element is
-- the second element of the pair that was removed.
removeContext :: [(a, b)] -> [([a], b)]
removeContext xs =
  mapMaybe
    (maybe2nd . (map fst *** fmap snd) . (`removeNth` xs))
    [0 .. (length xs - 1)]
  where
    maybe2nd :: (a, Maybe b) -> Maybe (a, b)
    maybe2nd (x, y) = (x,) <$> y

-- Remove and return the n-th element of a list.
removeNth :: Integer -> [a] -> ([a], Maybe a)
removeNth _ [] = ([], Nothing)
removeNth 0 (x : xs) = (xs, Just x)
removeNth n (x : xs) = first (x :) $ removeNth (n - 1) xs

extractValue :: TransactionConfig -> ValueType -> Value
extractValue conf = \case
  GeneralValue val -> val
  TokensValue tokenName pos ->
    Value.singleton
      (testCurrencySymbol conf)
      tokenName
      (getPositive pos)

module Test.Tasty.Plutus.Internal.Context (
  Purpose (..),
  ExternalType (..),
  Input (..),
  Output (..),
  Minting (..),
  TransactionConfig (..),
  ContextBuilder (..),
  compileSpending,
  compileMinting,
  Tokens (Tokens, unTokens),
  token,
  makeIncompleteContexts,
) where

import Control.Arrow ((***))
import Data.Bifunctor (first)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NonEmpty
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
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Prelude (length)
import Test.Tasty.Plutus.Options (ScriptInputPosition (Head, Tail))
import Prelude hiding (length)

{- | Describes what kind of script this is meant to test. Directly
 corresponds to 'ScriptPurpose'.

 @since 1.0
-}
data Purpose
  = -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Minting'. This tag applies
    -- to minting-policy-related structures and functions.
    --
    -- @since 1.0
    ForMinting
  | -- | Corresponds to 'Plutus.V1.Ledger.Contexts.Spending'. This tag applies
    -- to validator-related structures and functions.
    --
    -- @since 1.0
    ForSpending

{- | \'Marker type\' for input and output metadata.

 @since 3.0
-}
data ExternalType
  = -- | @since 3.0
    PubKeyType PubKeyHash
  | -- | @since 3.0
    ScriptType ValidatorHash BuiltinData
  | -- | @since 3.0
    OwnType BuiltinData
  deriving stock
    ( -- | @since 3.0
      Show
    )

{- | An input to a script, consisting of a value and a type.

 @since 1.0
-}
data Input
  = -- | @since 3.0
    Input ExternalType Value
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | An output from a script, consisting of a value and a type.

 @since 1.0
-}
data Output
  = -- | @since 3.0
    Output ExternalType Value
  deriving stock
    ( -- | @since 1.0
      Show
    )

{- | A minting result. Do not use this for tokens being minted by the current
 minting policy; pass those as 'Tokens' to the test instead.

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
data ContextBuilder (p :: Purpose)
  = ContextBuilder
      (Seq Input)
      (Seq Output)
      (Seq PubKeyHash)
      (Seq BuiltinData)
      (Seq Minting)

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
  forall (datum :: Type).
  (ToData datum) =>
  TransactionConfig ->
  ContextBuilder 'ForSpending ->
  datum ->
  Value ->
  ScriptContext
compileSpending conf cb d val =
  ScriptContext go
    . Spending
    . TxOutRef (testTxId conf)
    $ 0
  where
    go :: TxInfo
    go =
      let dt = toBuiltinData d
          baseInfo = baseTxInfo conf cb
          inInfo = createTxInInfo conf (0, Input (OwnType dt) val)
          inData = datumWithHash dt
       in baseInfo
            { txInfoInputs = case scriptInputPosition conf of
                Head -> inInfo : txInfoInputs baseInfo
                Tail -> txInfoInputs baseInfo <> [inInfo]
            , txInfoData = inData : txInfoData baseInfo
            }

compileMinting ::
  TransactionConfig ->
  ContextBuilder 'ForMinting ->
  Tokens ->
  ScriptContext
compileMinting conf cb (Tokens toks) =
  ScriptContext go (Minting sym)
  where
    go :: TxInfo
    go =
      let baseInfo = baseTxInfo conf cb
       in baseInfo
            { txInfoMint =
                Value.Value (Map.singleton sym toks) <> txInfoMint baseInfo
            }

    sym = testCurrencySymbol conf

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

     This can then be run in a `withValidator` block
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
    nonEmpty1st :: ([a], b) -> Maybe (NonEmpty.NonEmpty a, b)
    nonEmpty1st (xs, y) = (,y) <$> NonEmpty.nonEmpty xs

-- Helpers

baseTxInfo ::
  forall (p :: Purpose).
  TransactionConfig ->
  ContextBuilder p ->
  TxInfo
baseTxInfo conf (ContextBuilder ins outs pkhs dats mints) =
  let valHash = testValidatorHash conf
   in TxInfo
        { txInfoInputs = createTxInInfo conf <$> indexedInputs
        , txInfoOutputs = toList . fmap (toTxOut valHash) $ outs
        , txInfoFee = testFee conf
        , txInfoMint = foldMap unMint mints
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

    unMint (Mint val) = val

toInputDatum :: Input -> Maybe (DatumHash, Datum)
toInputDatum (Input typ _) = case typ of
  ScriptType _ dt -> Just . datumWithHash $ dt
  OwnType dt -> Just . datumWithHash $ dt
  PubKeyType _ -> Nothing

toOutputDatum :: Output -> Maybe (DatumHash, Datum)
toOutputDatum (Output typ _) = case typ of
  ScriptType _ dt -> Just . datumWithHash $ dt
  OwnType dt -> Just . datumWithHash $ dt
  PubKeyType _ -> Nothing

datumWithHash :: BuiltinData -> (DatumHash, Datum)
datumWithHash dt = (datumHash dt', dt')
  where
    dt' :: Datum
    dt' = Datum dt

createTxInInfo :: TransactionConfig -> (Integer, Input) -> TxInInfo
createTxInInfo conf (ix, Input typ v) =
  TxInInfo (TxOutRef (testTxId conf) ix) $ case typ of
    PubKeyType pkh -> TxOut (pubKeyHashAddress pkh) v Nothing
    ScriptType hash dat ->
      TxOut (scriptHashAddress hash) v . justDatumHash $ dat
    OwnType dat ->
      TxOut (scriptHashAddress . testValidatorHash $ conf) v . justDatumHash $ dat

justDatumHash :: BuiltinData -> Maybe DatumHash
justDatumHash = Just . datumHash . Datum

toTxOut :: ValidatorHash -> Output -> TxOut
toTxOut valHash (Output typ v) = case typ of
  PubKeyType pkh -> TxOut (pubKeyHashAddress pkh) v Nothing
  ScriptType hash dat ->
    TxOut (scriptHashAddress hash) v . justDatumHash $ dat
  OwnType dat ->
    TxOut (scriptHashAddress valHash) v . justDatumHash $ dat

{- | Tokens to be minted by minting policy.

  -- This type is 'Semigroup' but not 'Monoid', as a minting policy cannot be
  -- triggered if no tokens are minted.

  @since 4.1
-}
newtype Tokens = Tokens {unTokens :: Map TokenName Integer}
  deriving stock
    ( -- | @since 4.1
      Eq
    , -- | @since 4.1
      Show
    )

-- | @since 4.1
instance Semigroup Tokens where
  Tokens a <> Tokens b = Tokens (Map.unionWith (+) a b)

{- | Helper function to specify tokens to be minted.

  Combine using the 'Semigroup' instance for 'Tokens'.

  @since 4.1
-}
token :: TokenName -> Integer -> Tokens
token name = Tokens . Map.singleton name

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

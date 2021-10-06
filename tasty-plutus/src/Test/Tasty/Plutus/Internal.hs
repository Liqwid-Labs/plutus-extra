module Test.Tasty.Plutus.Internal (
  WithScript (..),
  Purpose (..),
  ExternalType (..),
  Input (..),
  Output (..),
  Minting (..),
  ContextBuilder (..),
  TransactionConfig (..),
  compileSpending,
  compileMinting,
  PropertyTestCount (..),
  PropertyMaxSize (..),
  ourStyle,
  ScriptResult (..),
  testValidatorScript,
  testMintingPolicyScript,
) where

import Control.Monad.RWS.Strict (RWS)
import Control.Monad.Reader (MonadReader (ask, local))
import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (toList)
import Ledger.Scripts (
  Context,
  Redeemer,
  ScriptError,
  datumHash,
  runMintingPolicyScript,
  runScript,
 )
import Plutus.V1.Ledger.Address (pubKeyHashAddress, scriptHashAddress)
import Plutus.V1.Ledger.Contexts (
  ScriptContext (ScriptContext),
  ScriptPurpose (Minting, Spending),
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
 )
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Scripts (
  Datum (Datum),
  DatumHash,
  MintingPolicy,
  Validator,
  ValidatorHash,
 )
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.TxId (TxId (TxId))
import Plutus.V1.Ledger.Value (CurrencySymbol, TokenName, Value)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Builtins (BuiltinData)
import PlutusTx.IsData.Class (ToData (toBuiltinData))
import Safe (lastMay)
import Test.Tasty (TestTree)
import Test.Tasty.Options (
  IsOption (
    defaultValue,
    optionHelp,
    optionName,
    parseValue,
    showDefaultValue
  ),
 )
import Text.PrettyPrint (Style (lineLength), style)
import Text.Read (readMaybe)
import Prelude

{- | Provides a monadic API for composing tests against the same validator or
 minting policy. While it has all the capabilities of a monad, you mostly
 won't need them. An example of the intended usage is:

 > withValidator "Testing my validator" myValidator $ do
 >    shouldValidate "Valid case" validData validContext
 >    shouldn'tValidate "Invalid context" validData invalidContext
 >    shouldn'tValidate "Invalid data" invalidData validContext
 >    shouldn'tValidate "Everything is bad" invalidData invalidContext
 >    scriptProperty "Some property" myGenerator mkContext
 >    ...

 'withMintingPolicy' works similarly.

 @since 3.0
-}
data WithScript (p :: Purpose) (a :: Type) where
  WithSpending ::
    RWS Validator (Seq TestTree) () a ->
    WithScript 'ForSpending a
  WithMinting ::
    RWS MintingPolicy (Seq TestTree) () a ->
    WithScript 'ForMinting a

-- | @since 1.0
deriving stock instance Functor (WithScript p)

-- | @since 1.0
instance Applicative (WithScript 'ForSpending) where
  {-# INLINEABLE pure #-}
  pure = WithSpending . pure
  {-# INLINEABLE (<*>) #-}
  WithSpending fs <*> WithSpending xs = WithSpending (fs <*> xs)

-- | @since 1.0
instance Applicative (WithScript 'ForMinting) where
  {-# INLINEABLE pure #-}
  pure = WithMinting . pure
  {-# INLINEABLE (<*>) #-}
  WithMinting fs <*> WithMinting xs = WithMinting (fs <*> xs)

-- | @since 1.0
instance Monad (WithScript 'ForSpending) where
  {-# INLINEABLE (>>=) #-}
  WithSpending xs >>= f = WithSpending $ do
    x <- xs
    let (WithSpending ys) = f x
    ys

-- | @since 1.0
instance Monad (WithScript 'ForMinting) where
  {-# INLINEABLE (>>=) #-}
  WithMinting xs >>= f = WithMinting $ do
    x <- xs
    let (WithMinting ys) = f x
    ys

-- | @since 3.0
instance MonadReader Validator (WithScript 'ForSpending) where
  {-# INLINEABLE ask #-}
  ask = WithSpending ask
  {-# INLINEABLE local #-}
  local f (WithSpending comp) = WithSpending . local f $ comp

-- | @since 3.0
instance MonadReader MintingPolicy (WithScript 'ForMinting) where
  {-# INLINEABLE ask #-}
  ask = WithMinting ask
  {-# INLINEABLE local #-}
  local f (WithMinting comp) = WithMinting . local f $ comp

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

{- | A minting result.

 @since 3.0
-}
data Minting
  = -- | @since 3.0
    OwnMint TokenName Integer
  | -- | @since 3.0
    OtherMint Value
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
            { txInfoInputs = inInfo : txInfoInputs baseInfo
            , txInfoData = inData : txInfoData baseInfo
            }

compileMinting ::
  TransactionConfig ->
  ContextBuilder 'ForMinting ->
  ScriptContext
compileMinting conf cb =
  ScriptContext go
    . Minting
    . testCurrencySymbol
    $ conf
  where
    go :: TxInfo
    go = baseTxInfo conf cb

-- Helpers

baseTxInfo ::
  forall (p :: Purpose).
  TransactionConfig ->
  ContextBuilder p ->
  TxInfo
baseTxInfo conf (ContextBuilder ins outs pkhs dats mints) =
  let currSymb = testCurrencySymbol conf
      valHash = testValidatorHash conf
   in TxInfo
        { txInfoInputs = createTxInInfo conf <$> indexedInputs
        , txInfoOutputs = toList . fmap (toTxOut valHash) $ outs
        , txInfoFee = testFee conf
        , txInfoMint = foldMap (mintingToValue currSymb) mints
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

mintingToValue :: CurrencySymbol -> Minting -> Value
mintingToValue cs = \case
  OwnMint tn i -> Value.singleton cs tn i
  OtherMint val -> val

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

{- | The number of property tests to run.

 This defaults to 1000. When passing this via CLI, use a positive integer
 only, and ensure that it's in bounds for 'Int'.

 @since 3.1
-}
newtype PropertyTestCount = PropertyTestCount Int
  deriving stock
    ( -- | @since 3.1
      Show
    )

-- | @since 3.1
instance IsOption PropertyTestCount where
  defaultValue = PropertyTestCount 1000
  parseValue s = PropertyTestCount <$> (readMaybe s >>= go)
    where
      go :: Int -> Maybe Int
      go i = case signum i of
        (-1) -> Nothing
        0 -> Nothing
        _ -> Just i
  optionName = Tagged "property-test-count"
  optionHelp = Tagged "Number of property tests to run."
  showDefaultValue (PropertyTestCount i) = Just . show $ i

{- | The maximum size given to generators for properties.

 This defaults to 100 (same as QuickCheck). When passing this via CLI, use a
 positive integer only, and ensure that it's in bounds for 'Int'.

 @since 3.1
-}
newtype PropertyMaxSize = PropertyMaxSize Int
  deriving stock
    ( -- | @since 3.1
      Show
    )

-- | @since 3.1
instance IsOption PropertyMaxSize where
  defaultValue = PropertyMaxSize 100
  parseValue s = PropertyMaxSize <$> (readMaybe s >>= go)
    where
      go :: Int -> Maybe Int
      go i = case signum i of
        (-1) -> Nothing
        0 -> Nothing
        _ -> Just i
  optionName = Tagged "property-max-size"
  optionHelp = Tagged "Maximum size for generators used for property tests."
  showDefaultValue (PropertyMaxSize i) = Just . show $ i

ourStyle :: Style
ourStyle = style {lineLength = 80}

-- The result of parsing a log from a script emulation
data ScriptResult
  = ScriptPassed
  | ScriptFailed
  | NoOutcome
  | ParseFailed Text
  | InternalError Text
  deriving stock (Eq, Show)

testValidatorScript ::
  Context ->
  Validator ->
  Datum ->
  Redeemer ->
  Either ScriptError ScriptResult
testValidatorScript ctx val d r = case runScript ctx val d r of
  Left err -> Left err
  Right (_, logs) -> Right . parseLogs $ logs

testMintingPolicyScript ::
  Context ->
  MintingPolicy ->
  Redeemer ->
  Either ScriptError ScriptResult
testMintingPolicyScript ctx mp r = case runMintingPolicyScript ctx mp r of
  Left err -> Left err
  Right (_, logs) -> Right . parseLogs $ logs

parseLogs :: [Text] -> ScriptResult
parseLogs logs = case lastMay logs >>= Text.stripPrefix "tasty-plutus: " of
  Nothing -> NoOutcome
  Just "Pass" -> ScriptPassed
  Just "Fail" -> ScriptFailed
  Just t -> case Text.stripPrefix "Parse failed: " t of
    Nothing -> InternalError t
    Just t' -> ParseFailed t'

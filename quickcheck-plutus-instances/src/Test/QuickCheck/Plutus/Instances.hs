{-# OPTIONS_GHC -Wno-orphans #-}

{- | Module: Test.QuickCheck.Plutus.Instances
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Instances of various QuickCheck type classes for Plutus types. Where
 possible, we have instances of:

 * 'Arbitrary', 'Arbitrary1' and 'Arbitrary2', with shrinker support
 * 'CoArbitrary'
 * 'Function'

 Some types may lack certain instances; for example, 'SignedMessage' lacks an
 'Arbitrary1' instance, as we generally can't lift shrinking into it.

 This module defines no exports; to use it, import it like so:

 > import Test.QuickCheck.Plutus.Instances ()
-}
module Test.QuickCheck.Plutus.Instances () where

import Control.Monad (forM, guard)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Exts qualified as GHC
import Ledger.Scripts (datumHash)
import Plutus.Contract.Oracle (
  Observation (Observation),
  SignedMessage (SignedMessage),
 )
import Plutus.V1.Ledger.Address (Address (Address))
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes))
import Plutus.V1.Ledger.Credential (
  Credential (PubKeyCredential, ScriptCredential),
  StakingCredential (StakingHash, StakingPtr),
 )
import Plutus.V1.Ledger.Crypto (
  PubKey (PubKey),
  PubKeyHash (PubKeyHash),
  Signature (Signature),
 )
import Plutus.V1.Ledger.Scripts (
  Datum (Datum),
  DatumHash (DatumHash),
  ValidatorHash (ValidatorHash),
 )
import Plutus.V1.Ledger.Time (
  DiffMilliSeconds (DiffMilliSeconds),
  POSIXTime (POSIXTime),
 )
import Plutus.V1.Ledger.Tx (
  TxOut (TxOut),
  TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.TxId (TxId (TxId))
import Plutus.V1.Ledger.Value (
  AssetClass (AssetClass),
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
  Value (Value),
  flattenValue,
  singleton,
 )
import PlutusTx (
  Data (B, Constr, I, List, Map),
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins.Internal (
  BuiltinByteString (BuiltinByteString),
  BuiltinData (BuiltinData),
  BuiltinString (BuiltinString),
 )
import PlutusTx.Prelude qualified as PTx
import PlutusTx.Ratio qualified as Ratio
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  Arbitrary2 (liftArbitrary2, liftShrink2),
  CoArbitrary (coarbitrary),
 )
import Test.QuickCheck.Function (Function (function), functionMap)
import Test.QuickCheck.Gen (
  Gen,
  chooseInt,
  getSize,
  oneof,
  scale,
  sized,
  suchThat,
  variant,
  vectorOf,
 )
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Modifiers (NonNegative (NonNegative))
import Test.QuickCheck.Plutus.Modifiers (
  UniqueKeys (UniqueKeys),
  UniqueList (UniqueList),
  uniqueListOf,
 )

-- | @since 1.0
instance Arbitrary BuiltinByteString where
  arbitrary = BuiltinByteString <$> genByteString64
  shrink (BuiltinByteString bs) = BuiltinByteString <$> shrinkByteString64 bs

-- | @since 1.0
instance CoArbitrary BuiltinByteString where
  coarbitrary (BuiltinByteString bs) =
    coarbitrary (GHC.toList bs)

-- | @since 1.0
instance Function BuiltinByteString where
  function = functionMap into outOf
    where
      into :: BuiltinByteString -> [Word8]
      into (BuiltinByteString bs) = GHC.toList bs
      outOf :: [Word8] -> BuiltinByteString
      outOf = BuiltinByteString . GHC.fromList

-- | @since 1.0
instance Arbitrary Data where
  arbitrary = sized go
    where
      go :: Int -> Gen Data
      go s
        | s <= 0 =
          oneof
            [ I <$> arbitrary
            , B <$> genByteString64
            ]
        | otherwise =
          oneof
            [ I <$> arbitrary
            , B <$> genByteString64
            , List <$> scale (`quot` 2) arbitrary
            , Map <$> scale (`quot` 2) arbitrary
            , Constr <$> genNonNegative <*> scale (`quot` 2) arbitrary
            ]
  shrink = \case
    I i -> I <$> shrink i
    B bs -> B <$> shrinkByteString64 bs
    List ds -> List <$> shrink ds
    Map keyVals -> Map <$> shrink keyVals
    Constr ix ds -> Constr <$> shrinkNonNegative ix <*> shrink ds

-- | @since 1.0
instance CoArbitrary Data where
  coarbitrary dat gen = case dat of
    I i -> variant (0 :: Int) . coarbitrary i $ gen
    B bs -> variant (1 :: Int) . coarbitraryByteString64 bs $ gen
    List ds -> variant (2 :: Int) . coarbitrary ds $ gen
    Map keyVals -> variant (3 :: Int) . coarbitrary keyVals $ gen
    Constr ix ds -> variant (4 :: Int) . coarbitrary ix . coarbitrary ds $ gen

-- | @since 1.0
instance Function Data where
  function = functionMap into outOf
    where
      into ::
        Data ->
        Either
          (Either Integer [Word8])
          (Either [Data] (Either [(Data, Data)] (Integer, [Data])))
      into = \case
        I i -> Left . Left $ i
        B bs -> Left . Right . GHC.toList $ bs
        List ds -> Right . Left $ ds
        Map keyVals -> Right . Right . Left $ keyVals
        Constr ix ds -> Right . Right . Right $ (ix, ds)
      outOf ::
        Either
          (Either Integer [Word8])
          (Either [Data] (Either [(Data, Data)] (Integer, [Data]))) ->
        Data
      outOf = \case
        Left (Left i) -> I i
        Left (Right w8s) -> B . GHC.fromList $ w8s
        Right (Left ds) -> List ds
        Right (Right (Left keyVals)) -> Map keyVals
        Right (Right (Right (ix, ds))) -> Constr ix ds

-- | @since 1.5
deriving via Integer instance Arbitrary DiffMilliSeconds

-- | @since 1.5
deriving via Integer instance CoArbitrary DiffMilliSeconds

-- | @since 1.5
instance Function DiffMilliSeconds where
  function = functionMap into DiffMilliSeconds
    where
      into :: DiffMilliSeconds -> Integer
      into (DiffMilliSeconds i) = i

-- | @since 1.0
deriving via BuiltinByteString instance Arbitrary LedgerBytes

-- | @since 1.0
deriving via BuiltinByteString instance CoArbitrary LedgerBytes

-- | @since 1.0
instance Function LedgerBytes where
  function = functionMap into LedgerBytes
    where
      into :: LedgerBytes -> BuiltinByteString
      into (LedgerBytes bs) = bs

-- | @since 1.0
deriving via LedgerBytes instance Arbitrary PubKey

-- | @since 1.0
deriving via LedgerBytes instance CoArbitrary PubKey

-- | @since 1.0
instance Function PubKey where
  function = functionMap into PubKey
    where
      into :: PubKey -> LedgerBytes
      into (PubKey lb) = lb

-- | @since 1.0
deriving via (NonNegative Integer) instance Arbitrary POSIXTime

-- | @since 1.0
deriving via Integer instance CoArbitrary POSIXTime

-- | @since 1.0
instance Function POSIXTime where
  function = functionMap into POSIXTime
    where
      into :: POSIXTime -> Integer
      into (POSIXTime t) = t

-- | @since 1.0
instance Arbitrary1 Observation where
  liftArbitrary gen = Observation <$> gen <*> arbitrary
  liftShrink shr (Observation x t) =
    Observation <$> shr x <*> shrink t

-- | @since 1.0
instance (Arbitrary a) => Arbitrary (Observation a) where
  arbitrary = liftArbitrary arbitrary
  shrink = liftShrink shrink

-- | @since 1.0
instance (CoArbitrary a) => CoArbitrary (Observation a) where
  coarbitrary (Observation x t) = coarbitrary (x, t)

-- | @since 1.0
instance (Function a) => Function (Observation a) where
  function = functionMap into outOf
    where
      into :: Observation a -> (a, POSIXTime)
      into (Observation x t) = (x, t)
      outOf :: (a, POSIXTime) -> Observation a
      outOf = uncurry Observation

-- | @since 1.0
deriving via Data instance Arbitrary BuiltinData

-- | @since 1.0
deriving via Data instance CoArbitrary BuiltinData

-- | @since 1.0
instance Function BuiltinData where
  function = functionMap into BuiltinData
    where
      into :: BuiltinData -> Data
      into (BuiltinData dat) = dat

-- | @since 1.0
deriving via BuiltinByteString instance Arbitrary Signature

-- | @since 1.0
deriving via BuiltinByteString instance CoArbitrary Signature

-- | @since 1.0
instance Function Signature where
  function = functionMap into Signature
    where
      into :: Signature -> BuiltinByteString
      into (Signature bs) = bs

-- | @since 1.0
deriving via BuiltinByteString instance Arbitrary DatumHash

-- | @since 1.0
deriving via BuiltinByteString instance CoArbitrary DatumHash

-- | @since 1.0
instance Function DatumHash where
  function = functionMap into DatumHash
    where
      into :: DatumHash -> BuiltinByteString
      into (DatumHash dh) = dh

-- | @since 1.0
deriving via BuiltinData instance Arbitrary Datum

-- | @since 1.0
deriving via BuiltinData instance CoArbitrary Datum

-- | @since 1.0
instance Function Datum where
  function = functionMap into Datum
    where
      into :: Datum -> BuiltinData
      into (Datum d) = d

-- | @since 1.0
instance
  (Arbitrary a, ToData a, FromData a) =>
  Arbitrary (SignedMessage a)
  where
  arbitrary = do
    d <- Datum . toBuiltinData <$> arbitrary @a
    let dh = datumHash d
    SignedMessage <$> arbitrary <*> pure dh <*> pure d
  shrink (SignedMessage sig _ (Datum bd)) = do
    x <- maybeToList . fromBuiltinData @a $ bd
    x' <- shrink x
    let d' = Datum . toBuiltinData $ x'
    let dh' = datumHash d'
    SignedMessage <$> shrink sig <*> pure dh' <*> pure d'

-- | @since 1.0
instance
  (CoArbitrary a, UnsafeFromData a) =>
  CoArbitrary (SignedMessage a)
  where
  coarbitrary (SignedMessage sig _ (Datum bd)) gen = do
    let x = unsafeFromBuiltinData @a bd
    coarbitrary x . coarbitrary sig $ gen

-- | @since 1.0
instance
  (Function a, UnsafeFromData a, ToData a) =>
  Function (SignedMessage a)
  where
  function = functionMap into outOf
    where
      into :: SignedMessage a -> (Signature, a)
      into (SignedMessage sig _ (Datum bd)) =
        let x = unsafeFromBuiltinData @a bd
         in (sig, x)
      outOf :: (Signature, a) -> SignedMessage a
      outOf (sig, x) =
        let d = Datum . toBuiltinData $ x
            dh = datumHash d
         in SignedMessage sig dh d

-- | @since 1.0
deriving via BuiltinByteString instance Arbitrary TokenName

-- | @since 1.0
deriving via BuiltinByteString instance CoArbitrary TokenName

-- | @since 1.0
instance Function TokenName where
  function = functionMap into TokenName
    where
      into :: TokenName -> BuiltinByteString
      into (TokenName tn) = tn

-- | @since 1.0
deriving via BuiltinByteString instance Arbitrary TxId

-- | @since 1.0
deriving via BuiltinByteString instance CoArbitrary TxId

-- | @since 1.0
instance Function TxId where
  function = functionMap into TxId
    where
      into :: TxId -> BuiltinByteString
      into (TxId bs) = bs

-- | @since 1.0
deriving via BuiltinByteString instance Arbitrary ValidatorHash

-- | @since 1.0
deriving via BuiltinByteString instance CoArbitrary ValidatorHash

-- | @since 1.0
instance Function ValidatorHash where
  function = functionMap into ValidatorHash
    where
      into :: ValidatorHash -> BuiltinByteString
      into (ValidatorHash bs) = bs

-- | @since 1.0
deriving via BuiltinByteString instance Arbitrary PubKeyHash

-- | @since 1.0
deriving via BuiltinByteString instance CoArbitrary PubKeyHash

-- | @since 1.0
instance Function PubKeyHash where
  function = functionMap into PubKeyHash
    where
      into :: PubKeyHash -> BuiltinByteString
      into (PubKeyHash bs) = bs

-- | @since 1.0
deriving via BuiltinByteString instance Arbitrary CurrencySymbol

-- | @since 1.0
deriving via BuiltinByteString instance CoArbitrary CurrencySymbol

-- | @since 1.0
instance Function CurrencySymbol where
  function = functionMap into CurrencySymbol
    where
      into :: CurrencySymbol -> BuiltinByteString
      into (CurrencySymbol bs) = bs

-- | @since 1.1
instance Arbitrary TxOutRef where
  arbitrary = do
    NonNegative tidx <- arbitrary
    TxOutRef <$> arbitrary <*> pure tidx
  shrink (TxOutRef tid tidx) = do
    NonNegative tidx' <- shrink . NonNegative $ tidx
    TxOutRef <$> shrink tid <*> pure tidx'

-- | @since 1.0
instance CoArbitrary TxOutRef where
  coarbitrary (TxOutRef tid tidx) gen =
    coarbitrary tid . coarbitrary tidx $ gen

-- | @since 1.0
instance Function TxOutRef where
  function = functionMap into outOf
    where
      into :: TxOutRef -> (TxId, Integer)
      into (TxOutRef tid tidx) = (tid, tidx)
      outOf :: (TxId, Integer) -> TxOutRef
      outOf (tid, tidx) = TxOutRef tid tidx

-- | @since 1.1
instance (Arbitrary k, Arbitrary v) => Arbitrary (AssocMap.Map k v) where
  arbitrary = liftArbitrary2 arbitrary arbitrary
  shrink = liftShrink2 shrink shrink

-- | @since 1.1
instance (Arbitrary k) => Arbitrary1 (AssocMap.Map k) where
  liftArbitrary = liftArbitrary2 arbitrary
  liftShrink = liftShrink2 shrink

-- | @since 1.1
instance Arbitrary2 AssocMap.Map where
  liftArbitrary2 genKey genVal =
    AssocMap.fromList <$> liftArbitrary (liftArbitrary2 genKey genVal)
  liftShrink2 shrinkKey shrinkValue =
    fmap AssocMap.fromList
      . liftShrink (liftShrink2 shrinkKey shrinkValue)
      . AssocMap.toList

-- | @since 1.1
instance (CoArbitrary k, CoArbitrary v) => CoArbitrary (AssocMap.Map k v) where
  coarbitrary aMap gen = case AssocMap.toList aMap of
    [] -> variant (0 :: Int) gen
    (kv : kvs) -> variant (1 :: Int) . coarbitrary (kv, kvs) $ gen

-- | @since 1.1
instance (Function k, Function v) => Function (AssocMap.Map k v) where
  function = functionMap AssocMap.toList AssocMap.fromList

-- | @since 1.1
instance Arbitrary Value where
  arbitrary = do
    num <- log2 <$> getSize
    UniqueList css <- uniqueListOf num
    lst <- forM css $ \cs -> do
      UniqueList tns <- uniqueListOf num
      lst' <- forM tns $ \tn -> (tn,) <$> arbitrary `suchThat` (PTx./= PTx.zero)
      pure (cs, AssocMap.fromList lst')
    pure . Value . AssocMap.fromList $ lst
  shrink = map (foldMap (\(cs, tn, i) -> singleton cs tn i)) . shrink . flattenValue

-- | @since 1.1
deriving via
  (UniqueKeys CurrencySymbol (UniqueKeys TokenName Integer))
  instance
    CoArbitrary Value

-- | @since 1.1
instance Function Value where
  function = functionMap into outOf
    where
      into ::
        Value ->
        UniqueKeys CurrencySymbol (UniqueKeys TokenName Integer)
      into (Value v) = UniqueKeys . PTx.fmap UniqueKeys $ v
      outOf ::
        UniqueKeys CurrencySymbol (UniqueKeys TokenName Integer) ->
        Value
      outOf (UniqueKeys aMap) =
        Value . PTx.fmap (\(UniqueKeys aMap') -> aMap') $ aMap

-- | @since 1.1
instance Arbitrary Credential where
  arbitrary = oneof [PubKeyCredential <$> arbitrary, ScriptCredential <$> arbitrary]
  shrink = \case
    PubKeyCredential pk -> PubKeyCredential <$> shrink pk
    ScriptCredential vh -> ScriptCredential <$> shrink vh

-- | @since 1.1
instance CoArbitrary Credential where
  coarbitrary cred gen = case cred of
    PubKeyCredential pk -> variant (0 :: Int) . coarbitrary pk $ gen
    ScriptCredential vh -> variant (1 :: Int) . coarbitrary vh $ gen

-- | @since 1.1
instance Function Credential where
  function = functionMap into outOf
    where
      into :: Credential -> Either PubKeyHash ValidatorHash
      into = \case
        PubKeyCredential pk -> Left pk
        ScriptCredential vh -> Right vh
      outOf :: Either PubKeyHash ValidatorHash -> Credential
      outOf = \case
        Left pk -> PubKeyCredential pk
        Right vh -> ScriptCredential vh

-- | @since 1.1
instance Arbitrary StakingCredential where
  arbitrary =
    oneof
      [ StakingHash <$> arbitrary
      , go
      ]
    where
      go :: Gen StakingCredential
      go = do
        NonNegative i <- arbitrary
        StakingPtr i <$> arbitrary <*> arbitrary
  shrink = \case
    StakingHash cred -> StakingHash <$> shrink cred
    StakingPtr i j k -> do
      NonNegative i' <- shrink . NonNegative $ i
      StakingPtr i' <$> shrink j <*> shrink k

-- | @since 1.1
instance CoArbitrary StakingCredential where
  coarbitrary sCred gen = case sCred of
    StakingHash cred -> variant (0 :: Int) . coarbitrary cred $ gen
    StakingPtr i j k ->
      variant (1 :: Int)
        . coarbitrary i
        . coarbitrary j
        . coarbitrary k
        $ gen

-- | @since 1.1
instance Function StakingCredential where
  function = functionMap into outOf
    where
      into ::
        StakingCredential ->
        Either Credential (Integer, Integer, Integer)
      into = \case
        StakingHash cred -> Left cred
        StakingPtr i j k -> Right (i, j, k)
      outOf ::
        Either Credential (Integer, Integer, Integer) ->
        StakingCredential
      outOf = \case
        Left cred -> StakingHash cred
        Right (i, j, k) -> StakingPtr i j k

-- | @since 1.1
instance Arbitrary Address where
  arbitrary = Address <$> arbitrary <*> arbitrary
  shrink (Address cred sCred) = Address <$> shrink cred <*> shrink sCred

-- | @since 1.1
instance CoArbitrary Address where
  coarbitrary (Address cred sCred) gen =
    coarbitrary cred . coarbitrary sCred $ gen

-- | @since 1.1
instance Function Address where
  function = functionMap into outOf
    where
      into :: Address -> (Credential, Maybe StakingCredential)
      into (Address cred mScred) = (cred, mScred)
      outOf :: (Credential, Maybe StakingCredential) -> Address
      outOf (cred, mScred) = Address cred mScred

-- | @since 1.1
instance Arbitrary TxOut where
  arbitrary = do
    UniqueKeys valMap <- PTx.fmap go <$> arbitrary
    let val = Value valMap
    TxOut <$> arbitrary <*> pure val <*> arbitrary
    where
      go ::
        UniqueKeys TokenName (NonNegative Integer) ->
        AssocMap.Map TokenName Integer
      go (UniqueKeys aMap) = PTx.fmap (\(NonNegative i) -> i) aMap
  shrink (TxOut addr val mDatumHash) =
    TxOut <$> shrink addr <*> go val <*> shrink mDatumHash
    where
      go :: Value -> [Value]
      go (Value aMap) = do
        let uniqued = UniqueKeys . PTx.fmap UniqueKeys $ aMap
        UniqueKeys aMap' <- shrink uniqued
        let aMap'' = PTx.fmap (\(UniqueKeys m) -> m) aMap'
        guard (PTx.all (PTx.all (PTx.>= PTx.zero)) aMap'')
        pure . Value $ aMap''

-- | @since 1.1
instance CoArbitrary TxOut where
  coarbitrary (TxOut addr val mDatumHash) gen =
    coarbitrary addr . coarbitrary val . coarbitrary mDatumHash $ gen

-- | @since 1.1
instance Function TxOut where
  function = functionMap into outOf
    where
      into :: TxOut -> (Address, Value, Maybe DatumHash)
      into (TxOut addr val mDH) = (addr, val, mDH)
      outOf :: (Address, Value, Maybe DatumHash) -> TxOut
      outOf (addr, val, mDH) = TxOut addr val mDH

-- | @since 1.2
deriving via (CurrencySymbol, TokenName) instance Arbitrary AssetClass

-- | @since 1.2
deriving via (CurrencySymbol, TokenName) instance CoArbitrary AssetClass

-- | @since 1.2
instance Function AssetClass where
  function = functionMap into AssetClass
    where
      into :: AssetClass -> (CurrencySymbol, TokenName)
      into (AssetClass x) = x

-- | @since 1.2
deriving via Text instance Arbitrary BuiltinString

-- | @since 1.2
deriving via Text instance CoArbitrary BuiltinString

-- | @since 1.2
instance Function BuiltinString where
  function = functionMap into BuiltinString
    where
      into :: BuiltinString -> Text
      into (BuiltinString t) = t

-- | @since 1.4
instance Arbitrary Ratio.Rational where
  arbitrary = Ratio.fromGHC <$> arbitrary
  shrink = fmap Ratio.fromGHC . shrink . Ratio.toGHC

-- | @since 1.4
instance CoArbitrary Ratio.Rational where
  coarbitrary r gen = do
    let num = Ratio.numerator r
    let den = Ratio.denominator r
    coarbitrary num . coarbitrary den $ gen

-- | @since 1.4
instance Function Ratio.Rational where
  function = functionMap Ratio.toGHC Ratio.fromGHC

-- Helpers

-- Generates ByteStrings up to 64 bytes long
genByteString64 :: Gen ByteString
genByteString64 = sized go
  where
    go :: Int -> Gen ByteString
    go s = do
      len <- chooseInt (0, min 64 s)
      GHC.fromList <$> vectorOf len arbitrary

-- Corresponding shrinker for genByteString64
shrinkByteString64 :: ByteString -> [ByteString]
shrinkByteString64 bs = do
  xs' <- shrink . GHC.toList $ bs
  pure . GHC.fromList $ xs'

-- Corresponding coarbitrary for genByteString64
coarbitraryByteString64 ::
  forall (b :: Type).
  ByteString ->
  Gen b ->
  Gen b
coarbitraryByteString64 bs = coarbitrary (GHC.toList bs)

-- Generates 0 or above.
genNonNegative :: Gen Integer
genNonNegative = do
  NonNegative i <- arbitrary
  pure i

-- Corresponding shrinker for genNonNegative
shrinkNonNegative :: Integer -> [Integer]
shrinkNonNegative i = do
  NonNegative i' <- shrink . NonNegative $ i
  pure i'

-- Integer part of logarithm base 2
log2 :: Int -> Int
log2 n
  | n <= 1 = 0
  | otherwise = 1 + log2 (n `div` 2)

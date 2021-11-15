{-# OPTIONS_GHC -Wno-orphans #-}

module Test.QuickCheck.Plutus.Instances () where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Maybe (maybeToList)
import Data.Word (Word8)
import GHC.Exts qualified as GHC
import Ledger.Oracle (
  Observation (Observation),
  SignedMessage (SignedMessage),
 )
import Ledger.Scripts (datumHash)
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes))
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
import Plutus.V1.Ledger.Time (POSIXTime (POSIXTime))
import Plutus.V1.Ledger.Tx (TxOutRef (TxOutRef))
import Plutus.V1.Ledger.TxId (TxId (TxId))
import Plutus.V1.Ledger.Value (
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
 )
import PlutusTx (
  Data (B, Constr, I, List, Map),
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Builtins.Internal (
  BuiltinByteString (BuiltinByteString),
  BuiltinData (BuiltinData),
 )
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary (coarbitrary),
 )
import Test.QuickCheck.Function (Function (function), functionMap)
import Test.QuickCheck.Gen (
  Gen,
  chooseInt,
  oneof,
  scale,
  sized,
  variant,
  vectorOf,
 )
import Test.QuickCheck.Modifiers (NonNegative (NonNegative))

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

-- | @since 1.0
instance Arbitrary TxOutRef where
  arbitrary = TxOutRef <$> arbitrary <*> arbitrary
  shrink (TxOutRef tid tidx) = TxOutRef <$> shrink tid <*> shrink tidx

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

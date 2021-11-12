{-# OPTIONS_GHC -Wno-orphans #-}

module Test.QuickCheck.Plutus.Instances () where

import Ledger.Oracle (Observation (Observation))
import Plutus.V1.Ledger.Time (POSIXTime (POSIXTime))
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes))
import Plutus.V1.Ledger.Crypto (PubKey (PubKey), Signature (Signature))
import Data.Kind (Type)
import Test.QuickCheck.Modifiers (NonNegative (NonNegative))
import PlutusTx (Data (B, Constr, I, List, Map))
import Data.Word (Word8)
import GHC.Exts qualified as GHC
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString),
  BuiltinData (BuiltinData))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink), 
  CoArbitrary (coarbitrary), Arbitrary1 (liftArbitrary, liftShrink))
import Test.QuickCheck.Gen (sized, Gen, chooseInt, vectorOf, oneof,
  scale, variant)
import Test.QuickCheck.Function (Function (function), functionMap)
import Data.ByteString (ByteString)

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
        | s <= 0 = oneof [I <$> arbitrary, 
                          B <$> genByteString64]
        | otherwise = oneof [I <$> arbitrary,
                             B <$> genByteString64,
                             List <$> scale (`quot` 2) arbitrary,
                             Map <$> scale (`quot` 2) arbitrary,
                             Constr <$> genNonNegative <*> scale (`quot` 2) arbitrary]
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
      into :: Data -> 
              Either (Either Integer [Word8]) 
                     (Either [Data] (Either [(Data, Data)] (Integer, [Data])))
      into = \case
        I i -> Left . Left $ i
        B bs -> Left . Right . GHC.toList $ bs
        List ds -> Right . Left $ ds
        Map keyVals -> Right . Right . Left $ keyVals
        Constr ix ds -> Right . Right . Right $ (ix, ds)
      outOf :: Either (Either Integer [Word8]) 
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
coarbitraryByteString64 :: forall (b :: Type) . 
  ByteString -> Gen b -> Gen b
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

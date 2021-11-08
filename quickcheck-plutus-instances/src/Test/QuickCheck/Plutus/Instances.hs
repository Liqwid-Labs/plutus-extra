{-# OPTIONS_GHC -Wno-orphans #-}

module Test.QuickCheck.Plutus.Instances () where

import PlutusTx (Data)
import GHC.Exts qualified as GHC
import PlutusTx.Prelude qualified as PTx
import PlutusTx.Builtins.Internal qualified as Internal
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (Gen)
import Data.ByteString (ByteString)

-- | @since 1.0
instance Arbitrary PTx.BuiltinByteString where
  arbitrary = Internal.BuiltinByteString <$> genByteString
  shrink (Internal.BuiltinByteString bs) = 
    Internal.BuiltinByteString <$> shrinkByteString bs

-- | @since 1.0
instance Arbitrary Data where
  arbitrary = _
  shrink = _

-- Helpers

genByteString :: Gen ByteString
genByteString = GHC.fromList <$> arbitrary

shrinkByteString :: ByteString -> [ByteString]
shrinkByteString = fmap GHC.fromList . shrink . GHC.toList

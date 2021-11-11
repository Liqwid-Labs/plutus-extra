{-# OPTIONS_GHC -Wno-orphans #-}

module Test.QuickCheck.Plutus.Instances () where

import Data.Word (Word8)
import GHC.Exts qualified as GHC
import PlutusTx.Prelude qualified as PTx
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Function (Function (function), functionMap)
import Test.QuickCheck.Gen (Gen)
import Data.ByteString (ByteString)

-- | @since 1.0
instance Arbitrary PTx.BuiltinByteString where
  arbitrary = BuiltinByteString <$> genByteString
  shrink (BuiltinByteString bs) = BuiltinByteString <$> shrinkByteString bs

-- | @since 1.0
instance Function PTx.BuiltinByteString where
  function = functionMap intoByteList outOfByteList
    where
      intoByteList :: BuiltinByteString -> [Word8]
      intoByteList (BuiltinByteString bs) = GHC.toList bs
      outOfByteList :: [Word8] -> BuiltinByteString
      outOfByteList = BuiltinByteString . GHC.fromList

-- Helpers

genByteString :: Gen ByteString
genByteString = GHC.fromList <$> arbitrary

shrinkByteString :: ByteString -> [ByteString]
shrinkByteString = fmap GHC.fromList . shrink . GHC.toList

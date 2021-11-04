{-# LANGUAGE QuasiQuotes #-}

module Test.Tasty.Plutus.Script.Size (
  -- * Test API
  fitsOnChain,
  fitsInto,

  -- * Byte size helper type
  ByteSize,
  bytes,
  kbytes,
) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BS
import Data.Int (Int64)
import Data.Tagged (Tagged (Tagged))
import Plutus.V1.Ledger.Scripts (Script)
import PlutusTx.Natural (Natural, nat)
import PlutusTx.Numeric.Extra (addExtend, divMod)
import PlutusTx.Prelude qualified as PTx
import Test.Tasty (TestTree)
import Test.Tasty.Plutus.Internal (ourStyle)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
  testPassed,
 )
import Text.PrettyPrint (renderStyle, (<+>))
import Text.Show.Pretty (dumpDoc)
import Prelude hiding (divMod)

-- | @since 3.4
fitsOnChain ::
  String ->
  Script ->
  TestTree
fitsOnChain scriptName =
  singleTest (scriptName <> " fits on-chain") . FitsOnChain

-- | @since 3.4
fitsInto ::
  String ->
  ByteSize ->
  Script ->
  TestTree
fitsInto scriptName maxSize =
  singleTest (scriptName <> " fits into " <> prettyByteSize maxSize)
    . FitsInto maxSize

-- | @since 3.4
newtype ByteSize = ByteSize Natural
  deriving
    ( -- | @since 3.4
      Eq
    , -- | @since 3.4
      Show
    )
    via Natural

-- | @since 3.4
bytes :: Natural -> ByteSize
bytes = ByteSize

-- | @since 3.4
kbytes :: Natural -> ByteSize
kbytes = ByteSize . (PTx.* [nat| 1024 |])

-- Helpers

prettyByteSize :: ByteSize -> String
prettyByteSize (ByteSize n) = case n `divMod` [nat| 1024 |] of
  (d, r) ->
    renderStyle ourStyle $
      if r == PTx.zero then dumpDoc d <> "KiB" else dumpDoc n <> "B"

data FitTest = FitsOnChain Script | FitsInto ByteSize Script

instance IsTest FitTest where
  run _ ft _ = pure $ case ft of
    FitsOnChain script ->
      let serializedSize = BS.length . serialise $ script
          limit = 16 * 1024 -- 16KiB is the current limit for on-chain
       in case compare serializedSize limit of
            GT -> testFailed . produceSize $ serializedSize
            _ -> testPassed . produceSize $ serializedSize
    FitsInto (ByteSize maxSize) script ->
      let serializedSize = BS.length . serialise $ script
          limit = fromIntegral . addExtend $ maxSize
       in case compare serializedSize limit of
            GT -> testFailed . produceSize $ serializedSize
            _ -> testPassed . produceSize $ serializedSize
    where
      produceSize :: Int64 -> String
      produceSize i = renderStyle ourStyle $ case i `quotRem` 1024 of
        (d, 0) -> "Size:" <+> dumpDoc d <+> "KiB"
        _ -> "Size:" <+> dumpDoc i <+> "B"
  testOptions = Tagged []

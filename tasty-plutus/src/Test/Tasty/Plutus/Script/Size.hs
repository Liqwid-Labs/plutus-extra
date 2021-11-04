{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
 Module: Test.Tasty.Plutus.Script.Size
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Provides a means of checking that a script fits into a given size, either
 based on a user-specified limit, or the inherent limit of the on-chain size
 (as currently known).

 = Example usage

 > mySizeTests :: TestTree
 > mySizeTests = testGroup "Size" [
 >    fitsOnChain myScript,
 >    fitsInto (kbytes [nat| 3 |]) myOtherScript,
 >    ...
-}
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

{- | Checks whether the given script fits on-chain given current limits (16KiB).

 = Note

 This does not include any arguments to the script; thus, passing this test
 isn't necessarily a guarantee that the script /and/ its arguments will fit.
 To assist with judging this, a successful test will also output the size.

 @since 3.4
-}
fitsOnChain ::
  String ->
  Script ->
  TestTree
fitsOnChain scriptName =
  singleTest (scriptName <> " fits on-chain") . FitsOnChain

{- | Checks whether the given script is no bigger than the given size limit.

 = Note

 This does not include any arguments to the script; thus, even if the script
 fits into that limit, its arguments will add to that size. To assist with
 judging this, a successful test will also output the size.

 @since 3.4
-}
fitsInto ::
  String ->
  ByteSize ->
  Script ->
  TestTree
fitsInto scriptName maxSize =
  singleTest (scriptName <> " fits into " <> prettyByteSize maxSize)
    . FitsInto maxSize

{- | An opaque type to represent sizes in bytes.

 If you want to construct a 'ByteSize', use either 'bytes' (for a number of
 bytes) or 'kbytes' (for a number of kibibytes, or 1024-byte blocks).

 @since 3.4
-}
newtype ByteSize = ByteSize Natural
  deriving
    ( -- | @since 3.4
      Eq
    , -- | @since 3.4
      Show
    )
    via Natural

{- | Yield a 'ByteSize' representing the given number of bytes.

 @since 3.4
-}
bytes :: Natural -> ByteSize
bytes = ByteSize

{- | Yield a 'ByteSize' representing the given number of kibibytes (that is,
 units of 1024 bytes).

 @since 3.4
-}
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
      produceSize i =
        renderStyle ourStyle $
          "Size:" <+> case i `quotRem` 1024 of
            (d, 0) -> dumpDoc d <> "KiB"
            (d, r) ->
              dumpDoc i <> "B (~"
                <> ( if
                        | r <= 256 -> dumpDoc d <> "KiB)"
                        | r > 256 && r < 768 -> dumpDoc d <> ".5KiB)"
                        | otherwise -> dumpDoc (d + 1) <> "KiB"
                   )
  testOptions = Tagged []

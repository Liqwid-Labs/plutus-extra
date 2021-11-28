module Test.Tasty.Plutus.Internal (
  ByteSize (..),
  byteSizeInt,
) where

{- | An opaque type to represent sizes in bytes.

 If you want to construct a 'ByteSize', use either
 'Test.Tasty.Plutus.QQ.bytes' (for a number of bytes) or
 'Test.Tasty.Plutus.QQ.kbytes' (for a number of kibibytes, or 1024-byte
 blocks).

 @since 1.0
-}
newtype ByteSize = ByteSize Int
  deriving
    ( -- | @since 1.0
      Eq
    , -- | @since 1.0
      Show
    , -- | @since 1.0
      Ord
    )
    via Int

{- | Currently, the on-chain size limit is 16KiB (or 16384 bytes).

 @since 1.0
-}
instance Bounded ByteSize where
  minBound = ByteSize 0
  maxBound = ByteSize 16384

{- | Converts a 'ByteSize' into its underlying 'Int'.

 @since 1.0
-}
byteSizeInt :: ByteSize -> Int
byteSizeInt (ByteSize i) = i

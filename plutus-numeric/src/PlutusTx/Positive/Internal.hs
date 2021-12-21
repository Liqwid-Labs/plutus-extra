{-# LANGUAGE TemplateHaskell #-}

module PlutusTx.Positive.Internal (
  Positive (Positive),
  getPositive,
) where

import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude
import Prelude qualified

import Control.Monad (guard)
import Data.Aeson (FromJSON (parseJSON), ToJSON)
import Data.Kind (Type)
import Data.OpenApi.Schema (ToSchema)
import PlutusTx.Builtins (matchData)
import PlutusTx.IsData (
  FromData (fromBuiltinData),
  ToData,
  UnsafeFromData (unsafeFromBuiltinData),
 )
import Schema qualified as PlutusSchema
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary,
  suchThat,
 )
import Test.QuickCheck.Function (Function (function), functionMap)
import Text.Show.Pretty (PrettyVal (prettyVal))

{- | A positive number.

 @since 4.1
-}
newtype Positive = Positive Integer
  deriving
    ( -- | @since 4.1
      AdditiveSemigroup
    , -- | @since 4.1
      MultiplicativeSemigroup
    , -- | @since 4.1
      MultiplicativeMonoid
    , -- | @since 4.1
      Eq
    , -- | @since 4.1
      Ord
    , -- | @since 4.1
      ToData
    , -- | @since 4.1
      ToJSON
    , -- | @since 4.1
      Prelude.Eq
    , -- | @since 4.1
      Prelude.Ord
    , -- | @since 4.1
      ToSchema
    , -- | @since 4.1
      CoArbitrary
    , -- | @since 4.1
      PlutusSchema.ToSchema
    , -- | @since 4.1
      PlutusSchema.ToArgument
    )
    via Integer
  deriving stock
    ( -- | @since 4.1
      Prelude.Show
    )

{- | Displays like a positive integer.

 @since 4.1
-}
instance PrettyVal Positive where
  {-# INLINEABLE prettyVal #-}
  prettyVal (Positive i) = prettyVal i

-- | @since 4.1
instance FromJSON Positive where
  parseJSON v = Prelude.fmap Positive $ do
    i <- parseJSON v
    guard (i > 0)
    Prelude.pure i

-- | @since 4.1
instance FromData Positive where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat =
    matchData
      dat
      (const go)
      go
      go
      (\i -> if i <= zero then Nothing else Just . Positive $ i)
      go
    where
      go :: forall (a :: Type). a -> Maybe Positive
      go = const Nothing

-- | @since 4.1
instance UnsafeFromData Positive where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let i = unsafeFromBuiltinData dat
     in if i <= zero then error () else Positive i

{- | This is partial all over the place, but so is 'Enum' for most things.

 @since 4.1
-}
instance Enum Positive where
  {-# INLINEABLE succ #-}
  succ (Positive n) = Positive (n + 1)
  {-# INLINEABLE pred #-}
  pred (Positive n)
    | n <= 1 = error . trace "No predecessor to Positive 1" $ ()
    | otherwise = Positive (n - 1)
  {-# INLINEABLE toEnum #-}
  toEnum i
    | i <= 0 = error . trace "Cannot convert a non-positive number to a Positive" $ ()
    | otherwise = Positive i
  {-# INLINEABLE fromEnum #-}
  fromEnum (Positive n) = n

-- | @since 4.1
instance Arbitrary Positive where
  arbitrary = Positive . Prelude.abs Prelude.<$> arbitrary `suchThat` (/= 0)
  shrink (Positive i) = Positive Prelude.<$> (Prelude.filter (> 0) . shrink $ i)

-- | @since 4.1
instance Function Positive where
  function = functionMap into Positive
    where
      into :: Positive -> Integer
      into (Positive i) = i

{- | Extract integer from Positive

 @since 4.1
-}
{-# INLINEABLE getPositive #-}
getPositive :: Positive -> Integer
getPositive (Positive i) = i

makeLift ''Positive

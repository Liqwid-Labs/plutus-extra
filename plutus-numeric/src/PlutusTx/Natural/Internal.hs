{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module PlutusTx.Natural.Internal (
  -- * Types,
  Natural (..),
  Parity (..),

  -- * Functions
  parity,
) where

import Control.Monad (guard)
import Data.Aeson (FromJSON (parseJSON), ToJSON)
import Data.OpenApi.Schema qualified as OpenApi
import PlutusTx.Builtins (matchData, unsafeDataAsI)
import PlutusTx.IsData (
  FromData (fromBuiltinData),
  ToData,
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude hiding (even)
import Schema (ToArgument, ToSchema)
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  CoArbitrary,
 )
import Test.QuickCheck.Function (Function (function), functionMap)
import Text.Show.Pretty (PrettyVal (prettyVal), Value (Con))
import Prelude qualified

{- | A non-negative number.

 @since 1.0
-}
newtype Natural = Natural Integer
  deriving
    ( -- | @since 1.0
      AdditiveSemigroup
    , -- | @since 1.0
      AdditiveMonoid
    , -- | @since 1.0
      MultiplicativeSemigroup
    , -- | @since 1.0
      MultiplicativeMonoid
    , -- | @since 1.0
      Eq
    , -- | @since 1.0
      Ord
    , -- | @since 1.0
      ToData
    , -- | @since 1.0
      ToJSON
    , -- | @since 1.0
      ToSchema
    , -- | @since 1.0
      ToArgument
    , -- | @since 1.0
      Prelude.Eq
    , -- | @since 1.1
      Prelude.Ord
    , -- | @since 1.0
      OpenApi.ToSchema
    , -- | @since 2.2
      CoArbitrary
    )
    via Integer
  deriving stock
    ( -- | @since 1.0
      Prelude.Show
    )

{- | Displays like a positive integer.

 @since 1.0
-}
instance PrettyVal Natural where
  {-# INLINEABLE prettyVal #-}
  prettyVal (Natural i) = prettyVal i

-- | @since 1.0
instance FromJSON Natural where
  parseJSON v = Prelude.fmap Natural $ do
    i <- parseJSON v
    guard (i >= 0)
    Prelude.pure i

-- | @since 1.0
instance FromData Natural where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat =
    matchData
      dat
      (\_ -> const Nothing)
      (const Nothing)
      (const Nothing)
      go
      (const Nothing)
    where
      go :: Integer -> Maybe Natural
      go x
        | x < zero = Nothing
        | otherwise = Just . Natural $ x

-- | @since 1.0
instance UnsafeFromData Natural where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let asI = unsafeDataAsI dat
     in if asI >= 0
          then Natural asI
          else error . trace "Cannot decode a negative value to Natural" $ ()

{- | This is partial all over the place, but so is 'Enum' for most things.

 @since 1.0
-}
instance Enum Natural where
  {-# INLINEABLE succ #-}
  succ (Natural n) = Natural (n + 1)
  {-# INLINEABLE pred #-}
  pred (Natural n)
    | n <= 0 = error . trace "No predecessor to Natural 0" $ ()
    | otherwise = Natural (n - 1)
  {-# INLINEABLE toEnum #-}
  toEnum i
    | i < 0 = error . trace "Cannot convert a negative number to a Natural" $ ()
    | otherwise = Natural i
  {-# INLINEABLE fromEnum #-}
  fromEnum (Natural n) = n

-- | @since 1.0
instance Arbitrary Natural where
  arbitrary = Natural . Prelude.abs Prelude.<$> arbitrary
  shrink (Natural i) = Natural Prelude.<$> (Prelude.filter (> 0) . shrink $ i)

-- | @since 2.2
instance Function Natural where
  function = functionMap into Natural
    where
      into :: Natural -> Integer
      into (Natural i) = i

{- | A demonstration of the parity of a number.

 @since 1.0
-}
data Parity = Even | Odd
  deriving stock
    ( -- | @since 1.0
      Prelude.Eq
    , -- | @since 1.0
      Prelude.Show
    )

-- | @since 1.0
instance PrettyVal Parity where
  {-# INLINEABLE prettyVal #-}
  prettyVal = \case
    Even -> Con "Even" []
    Odd -> Con "Odd" []

{- | Determine the parity of a number.

 @since 1.0
-}
parity :: Natural -> Parity
parity (Natural n)
  | even n = Even
  | otherwise = Odd
  where
    even :: Integer -> Bool
    even x = x `modulo` 2 == 0

makeLift ''Natural

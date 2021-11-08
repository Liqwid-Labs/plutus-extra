{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module PlutusTx.NatRatio.Internal (
  NatRatio (..),
  natRatio,
  fromNatural,
  numerator,
  denominator,
  truncate,
  ceiling,
  PlutusTx.NatRatio.Internal.round,
  properFraction,
  recip,
  toRational,
  NatRatioSchema (..),
) where

import Control.Monad (guard)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value,
  object,
  withObject,
  (.:),
 )
import Data.Aeson.Types (Parser)
import Data.Coerce
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol)
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData,
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Lift (makeLift)
import PlutusTx.Natural.Internal (Natural (Natural))
import PlutusTx.Prelude
import PlutusTx.Ratio qualified as Ratio
import PlutusTx.SchemaUtils (
  RatioDirection ((:->)),
  jsonFieldSym,
  ratioDeclareNamedSchema,
  ratioFixFormArgument,
  ratioFormSchema,
  ratioTypeName,
 )
import Schema (
  FormSchema,
  ToArgument,
  ToSchema,
  toArgument,
  toSchema,
 )
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (suchThat)
import Text.Show.Pretty (PrettyVal (prettyVal))
import Prelude qualified

{- | A ratio of 'Natural's. Similar to 'Rational', but with the numerator and
 denominator guaranteed non-negative.

 @since 1.0
-}
newtype NatRatio = NatRatio Rational
  deriving stock
    ( -- | @since 1.0
      Prelude.Show
    )
  deriving
    ( -- | @since 1.0
      Prelude.Eq
    , -- | @since 1.1
      Prelude.Ord
    , -- | @since 1.0
      Eq
    , -- | @since 1.0
      Ord
    , -- | @since 1.0
      AdditiveSemigroup
    , -- | @since 1.0
      AdditiveMonoid
    , -- | @since 1.0
      MultiplicativeSemigroup
    , -- | @since 1.0
      MultiplicativeMonoid
    , -- | @since 1.0
      ToData
    , -- | @since 1.0
      ToJSON
    , -- | @since 1.0
      ToArgument
    )
    via Rational
  deriving
    ( -- | @since @1.2
      ToSchema
    , -- | @since @1.2
      OpenApi.ToSchema
    )
    via (NatRatioSchema ("denominator" ':-> "numerator"))

{- | Represents this like a positive-only ratio.

 @since 1.0
-}
instance PrettyVal NatRatio where
  {-# INLINEABLE prettyVal #-}
  prettyVal (NatRatio r) = prettyVal . Ratio.toGHC $ r

-- | @since 1.0
instance FromJSON NatRatio where
  parseJSON v = do
    r <- parseJSON v
    guard (r >= zero)
    Prelude.pure . NatRatio $ r

-- | @since 1.0
instance FromData NatRatio where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = case fromBuiltinData dat of
    Nothing -> Nothing
    Just r ->
      if Ratio.abs r == r
        then Just (NatRatio r)
        else Nothing

-- | @since 1.0
instance UnsafeFromData NatRatio where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat =
    let r = unsafeFromBuiltinData dat
     in if Ratio.abs r == r
          then NatRatio r
          else error . trace "Negative fractions cannot be NatRatio" $ ()

-- | @since 1.0
instance Arbitrary NatRatio where
  arbitrary = do
    Natural num <- arbitrary
    Natural den <- suchThat arbitrary (> zero)
    Prelude.pure . NatRatio $ num Ratio.% den
  shrink nr = do
    let Natural num = numerator nr
    let Natural den = denominator nr
    num' <- Prelude.filter (> 0) . shrink $ num
    den' <- Prelude.filter (> 0) . shrink $ den
    Prelude.pure . NatRatio $ num' Ratio.% den'

{- | Safely construct a 'NatRatio'. Checks for a zero denominator.

 @since 1.0
-}
{-# INLINEABLE natRatio #-}
natRatio :: Natural -> Natural -> Maybe NatRatio
natRatio (Natural n) (Natural m) =
  if m == 0
    then Nothing
    else Just . NatRatio $ n Ratio.% m

{- | Convert a 'Natural' into a 'NatRatio' with the same value.

 @since 1.0
-}
{-# INLINEABLE fromNatural #-}
fromNatural :: Natural -> NatRatio
fromNatural (Natural n) = NatRatio $ n % 1

{- | Retrieve the numerator of a 'NatRatio'.

 @since 1.0
-}
{-# INLINEABLE numerator #-}
numerator :: NatRatio -> Natural
numerator (NatRatio r) = Natural . Ratio.numerator $ r

{- | Retrieve the denominator of a 'NatRatio'. This is guaranteed non-zero,
 though the result type doesn't specify this.

 @since 1.0
-}
{-# INLINEABLE denominator #-}
denominator :: NatRatio -> Natural
denominator (NatRatio r) = Natural . Ratio.denominator $ r

{- | Round the 'NatRatio' down.

 @since 1.0
-}
{-# INLINEABLE truncate #-}
truncate :: NatRatio -> Natural
truncate (NatRatio r) = Natural . Ratio.truncate $ r

{- | Round the 'NatRatio' arithmetically.

 @since 1.0
-}
{-# INLINEABLE round #-}
round :: NatRatio -> Natural
round (NatRatio r) = Natural . Ratio.round $ r

{- | Take the reciprocal of the 'NatRatio'.

 @since 1.0
-}
{-# INLINEABLE recip #-}
recip :: NatRatio -> NatRatio
recip (NatRatio r) = NatRatio . Ratio.recip $ r

{- | Round the 'NatRatio' up.

 @since 1.0
-}
{-# INLINEABLE ceiling #-}
ceiling :: NatRatio -> Natural
ceiling x =
  case properFraction x of
    (floor, leftover)
      | leftover == zero -> floor
      | otherwise -> succ floor

{- | Separate a 'NatRatio' into a whole and a fractional part, such that the
 fractional part is guaranteed to be less than @1@.

 @since 1.0
-}
{-# INLINEABLE properFraction #-}
properFraction :: NatRatio -> (Natural, NatRatio)
properFraction (NatRatio r) =
  let (n, r') = Ratio.properFraction r
   in (Natural n, NatRatio r')

{- | Convert a 'NatRatio' to the underlying 'Rational'.

 @since 1.0
-}
toRational :: NatRatio -> Rational
toRational (NatRatio r) = r

{- | Newtype for deriving Schema and JSON instances

 @since 1.2
-}
newtype NatRatioSchema (dir :: RatioDirection)
  = NatRatioSchema NatRatio
  deriving stock (Prelude.Show, Generic)

-- | @since 1.2
instance
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol to
  , KnownSymbol from
  ) =>
  ToJSON (NatRatioSchema (from ':-> to))
  where
  toJSON :: NatRatioSchema (from ':-> to) -> Value
  toJSON (NatRatioSchema ratio) =
    object
      [ (jsonFieldSym @from, toJSON @Natural $ denominator ratio)
      , (jsonFieldSym @to, toJSON @Natural $ numerator ratio)
      ]

-- | @since 1.2
instance
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol to
  , KnownSymbol from
  ) =>
  FromJSON (NatRatioSchema (from ':-> to))
  where
  parseJSON :: Value -> Parser (NatRatioSchema (from ':-> to))
  parseJSON =
    withObject (ratioTypeName @from @to "NatRatio") $ \obj -> do
      from' <- obj .: jsonFieldSym @from
      to' <- obj .: jsonFieldSym @to
      case natRatio to' from' of
        Nothing -> Prelude.fail "Zero is not a valid NatRatio denominator"
        Just nr -> Prelude.pure . NatRatioSchema $ nr

-- | @since 1.2
instance
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol from
  , KnownSymbol to
  ) =>
  ToSchema (NatRatioSchema (from ':-> to))
  where
  toSchema :: FormSchema
  toSchema = ratioFormSchema @from @to

-- | @since 1.2
instance
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol from
  , KnownSymbol to
  ) =>
  ToArgument (NatRatioSchema (from ':-> to))
  where
  toArgument (NatRatioSchema ratio) =
    ratioFixFormArgument @from @to fromVal toVal
    where
      fromVal :: Integer
      fromVal = coerce . denominator $ ratio
      toVal :: Integer
      toVal = coerce . numerator $ ratio

-- | @since 1.2
instance
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol from
  , KnownSymbol to
  ) =>
  OpenApi.ToSchema (NatRatioSchema (from ':-> to))
  where
  declareNamedSchema _ =
    ratioDeclareNamedSchema @from @to "NatRatioSchema"

makeLift ''NatRatio

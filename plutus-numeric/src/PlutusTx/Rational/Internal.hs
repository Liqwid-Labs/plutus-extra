{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PlutusTx.Rational.Internal (
  Rational (..),
  RatioSchema (..),
  (%),
  ratio,
  toGHC,
  numerator,
  denominator,
  euclid,
  rDiv,
  rPowInteger,
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
import Data.OpenApi (ToSchema (declareNamedSchema))
import Data.Ratio qualified as GHC
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol)
import PlutusTx.IsData.Class (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )
import PlutusTx.Lift (makeLift)
import PlutusTx.Prelude hiding (
  Rational,
  fromInteger,
  negate,
  round,
  (%),
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.SchemaUtils (
  RatioFields ((:%:)),
  jsonFieldSym,
  ratioDeclareNamedSchema,
  ratioFixFormArgument,
  ratioFormSchema,
  ratioTypeName,
 )
import Schema qualified as PlutusSchema
import Test.QuickCheck.Arbitrary (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
 )
import Test.QuickCheck.Function (Function (function), functionMap)
import Test.QuickCheck.Modifiers (Positive (Positive))
import Text.Show.Pretty (PrettyVal (prettyVal))
import Prelude qualified

{- | A data type representing an arbitrary-precision ratio.

 @since 4.0
-}
data Rational = Rational Integer Integer
  deriving stock
    ( -- | @since 4.0
      Prelude.Eq
    , -- | @since 4.0
      Prelude.Show
    )
  deriving
    ( -- | @since 4.0
      ToSchema
    , -- | @since 4.0
      PlutusSchema.ToSchema
    , -- | @since 4.0
      PlutusSchema.ToArgument
    )
    via (RatioSchema ("numerator" ':%: "denominator"))

-- | @since 4.0
instance Eq Rational where
  {-# INLINEABLE (==) #-}
  Rational n d == Rational n' d' = n == n' && d == d'

-- | @since 4.0
instance Ord Rational where
  {-# INLINEABLE compare #-}
  compare (Rational n d) (Rational n' d') = compare (n * d') (n' * d)
  {-# INLINEABLE (<=) #-}
  Rational n d <= Rational n' d' = (n * d') <= (n' * d)
  {-# INLINEABLE (>=) #-}
  Rational n d >= Rational n' d' = (n * d') >= (n' * d)
  {-# INLINEABLE (<) #-}
  Rational n d < Rational n' d' = (n * d') < (n' * d)
  {-# INLINEABLE (>) #-}
  Rational n d > Rational n' d' = (n * d') > (n' * d)

-- | @since 4.0
instance Prelude.Ord Rational where
  compare (Rational n d) (Rational n' d') =
    Prelude.compare (n Prelude.* d') (n' Prelude.* d)
  Rational n d <= Rational n' d' =
    (n Prelude.* d') Prelude.<= (n' Prelude.* d)
  Rational n d >= Rational n' d' =
    (n Prelude.* d') Prelude.>= (n' Prelude.* d)
  Rational n d < Rational n' d' =
    (n Prelude.* d') Prelude.< (n' Prelude.* d)
  Rational n d > Rational n' d' =
    (n Prelude.* d') Prelude.> (n' Prelude.* d)

-- | @since 4.0
instance AdditiveSemigroup Rational where
  {-# INLINEABLE (+) #-}
  Rational n d + Rational n' d' =
    let newNum = (n * d') + (n' * d)
        newDen = d * d'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

-- | @since 4.0
instance AdditiveMonoid Rational where
  {-# INLINEABLE zero #-}
  zero = Rational zero one

-- | @since 4.0
instance AdditiveGroup Rational where
  {-# INLINEABLE (-) #-}
  Rational n d - Rational n' d' =
    let newNum = (n * d') - (n' * d)
        newDen = d * d'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

-- | @since 4.0
instance MultiplicativeSemigroup Rational where
  {-# INLINEABLE (*) #-}
  Rational n d * Rational n' d' =
    let newNum = n * n'
        newDen = d * d'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

-- | @since 4.0
instance MultiplicativeMonoid Rational where
  {-# INLINEABLE one #-}
  one = Rational one one

-- | @since 4.2
instance Plutus.Module Integer Rational where
  {-# INLINEABLE scale #-}
  scale i (Rational n d) =
    let newNum = i * n
        gcd = euclid newNum d
     in Rational (newNum `quotient` gcd) (d `quotient` gcd)

-- | @since 4.0
instance Arbitrary Rational where
  arbitrary = do
    num <- arbitrary
    Positive den <- arbitrary
    Prelude.pure $ num % den
  shrink r@(Rational num den) = do
    num' <- shrink num
    Positive den' <- shrink . Positive $ den
    let res = num' % den'
    guard (res Prelude.< r)
    Prelude.pure res

-- | @since 4.0
instance CoArbitrary Rational where
  coarbitrary (Rational n d) = coarbitrary n . coarbitrary d

-- | @since 4.0
instance Function Rational where
  function = functionMap into outOf
    where
      into :: Rational -> (Integer, Integer)
      into (Rational n d) = (n, d)
      outOf :: (Integer, Integer) -> Rational
      outOf (n, d) = Rational n d

-- | @since 4.0
instance ToData Rational where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (Rational n d) = toBuiltinData (n, d)

-- | @since 4.0
instance FromData Rational where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData dat = do
    (n, d) <- fromBuiltinData dat
    guard (d /= zero)
    pure (n % d)

-- | @since 4.0
instance UnsafeFromData Rational where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData dat = case unsafeFromBuiltinData dat of
    (n, d) -> n % d

-- | @since 4.0
instance ToJSON Rational where
  toJSON (Rational n d) =
    object
      [ ("numerator", toJSON n)
      , ("denominator", toJSON d)
      ]

-- | @since 4.0
instance FromJSON Rational where
  parseJSON = withObject "Rational" $ \obj -> do
    n <- obj .: "numerator"
    d <- obj .: "denominator"
    if d == 0
      then Prelude.fail "Zero denominator is invalid."
      else Prelude.pure (n % d)

-- | @since 4.0
instance PrettyVal Rational where
  prettyVal = prettyVal . toGHC

{-# INLINEABLE (%) #-}
(%) :: Integer -> Integer -> Rational
n % d
  | d == zero = error ()
  | d < zero = Plutus.negate n % Plutus.negate d
  | otherwise =
    let gcd = euclid n d
     in Rational (n `quotient` gcd) (d `quotient` gcd)

infixl 7 %

{- | Safely constructs a 'Rational' from a numerator and denominator. Returns
 'Nothing' if given a zero denominator.

 @since 4.0
-}
{-# INLINEABLE ratio #-}
ratio :: Integer -> Integer -> Maybe Rational
ratio n d
  | d == zero = Nothing
  | d < zero = Just (Plutus.negate n % Plutus.negate d)
  | otherwise =
    let gcd = euclid n d
     in Just . Rational (n `quotient` gcd) $ d `quotient` gcd

{- | Converts a 'Rational' to a GHC 'GHC.Rational', preserving value. Does not
 work on-chain.

 @since 4.0
-}
toGHC :: Rational -> GHC.Rational
toGHC (Rational n d) = n GHC.% d

{- | Returns the (possibly reduced) numerator of its argument.

 @since 4.0
-}
{-# INLINEABLE numerator #-}
numerator :: Rational -> Integer
numerator (Rational n _) = n

{- | Returns the (possibly reduced) denominator of its argument. This will
 always be greater than 1, although the type does not describe this.

 @since 4.0
-}
{-# INLINEABLE denominator #-}
denominator :: Rational -> Integer
denominator (Rational _ d) = d

{- | Newtype for deriving 'ToSchema', 'ToJSON' and 'FromJSON' instances
  for newtypes over 'Rational' with the specified field names for the
  numerator and denominator.

= Example

@
newtype PercentageChangeRatio = PercentageChangeRatio Rational
  deriving
    (ToJSON, FromJSON, OpenApi.ToSchema)
    via (RatioSchema ("Change" ':%: "Value"))
@

 @since 2.3
-}
newtype RatioSchema (dir :: RatioFields)
  = RatioSchema Rational
  deriving stock
    ( -- | @since 2.3
      Prelude.Show
    , -- | @since 2.3
      Generic
    )

-- | @since 2.3
instance
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  ToJSON (RatioSchema (numerator ':%: denominator))
  where
  toJSON :: RatioSchema (numerator ':%: denominator) -> Value
  toJSON (RatioSchema r) =
    object
      [ (jsonFieldSym @numerator, toJSON @Integer $ numerator r)
      , (jsonFieldSym @denominator, toJSON @Integer $ denominator r)
      ]

-- | @since 2.3
instance
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  FromJSON (RatioSchema (numerator ':%: denominator))
  where
  parseJSON :: Value -> Parser (RatioSchema (numerator ':%: denominator))
  parseJSON =
    withObject (ratioTypeName @numerator @denominator "Ratio") $ \obj ->
      mkRatioSchema
        Prelude.<$> obj .: jsonFieldSym @numerator
        Prelude.<*> obj .: jsonFieldSym @denominator

-- | @since 2.3
instance
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  ToSchema (RatioSchema (numerator ':%: denominator))
  where
  declareNamedSchema _ =
    ratioDeclareNamedSchema @numerator @denominator "RatioSchema"

-- | @since 2.3
instance
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  PlutusSchema.ToSchema (RatioSchema (numerator ':%: denominator))
  where
  toSchema :: PlutusSchema.FormSchema
  toSchema = ratioFormSchema @numerator @denominator

-- | @since 2.3
instance
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  PlutusSchema.ToArgument (RatioSchema (numerator ':%: denominator))
  where
  toArgument (RatioSchema r) =
    ratioFixFormArgument @numerator @denominator num denom
    where
      num :: Integer
      num = numerator r
      denom :: Integer
      denom = denominator r

-- Euclid's algorithm
{-# INLINEABLE euclid #-}
euclid :: Integer -> Integer -> Integer
euclid x y
  | y == zero = x
  | otherwise = euclid y (x `modulo` y)

-- Helper for division
{-# INLINEABLE rDiv #-}
rDiv :: Rational -> Rational -> Rational
rDiv (Rational n d) (Rational n' d')
  | n' == zero = error ()
  | otherwise =
    let newNum = n * d'
        newDen = d * n'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

-- Helper for powInteger
{-# INLINEABLE rPowInteger #-}
rPowInteger :: Rational -> Integer -> Rational
rPowInteger (Rational n d) i
  | i < zero && n == zero = error ()
  | i == zero = one
  | otherwise =
    let (i', num, den) =
          if i < zero
            then (Plutus.negate i, d, n)
            else (i, n, d)
        newNum = expBySquaring num i'
        newDen = expBySquaring den i'
        gcd = euclid newNum newDen
     in Rational (newNum `quotient` gcd) (newDen `quotient` gcd)

-- Helpers

-- We secretly know the exponent is positive
{-# INLINEABLE expBySquaring #-}
expBySquaring :: Integer -> Integer -> Integer
expBySquaring acc i
  | i == one = acc
  | even i = expBySquaring (acc * acc) $ i `divide` 2
  | otherwise = (acc *) . expBySquaring acc $ i - 1

mkRatioSchema ::
  forall (numerator :: Symbol) (denominator :: Symbol).
  Integer ->
  Integer ->
  RatioSchema (numerator ':%: denominator)
mkRatioSchema num denom = RatioSchema (num % denom)

$(makeLift ''Rational)

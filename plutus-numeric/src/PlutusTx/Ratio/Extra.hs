module PlutusTx.Ratio.Extra (
  RatioSchema (RatioSchema),
) where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value,
  object,
  withObject,
  (.:),
 )
import Data.Aeson.Types (Parser)
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol)
import PlutusTx.Prelude qualified
import PlutusTx.Ratio qualified as Ratio
import PlutusTx.SchemaUtils (
  RatioFields ((:%:)),
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
import Prelude

{- | Newtype for deriving
'ToSchema', 'ToArgument', 'OpenApi.ToSchema', 'ToJSON' and 'FromJSON' instances
for newtypes over Rational with the specified field names for the numerator and denominator.

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
  = RatioSchema PlutusTx.Prelude.Rational
  deriving stock
    ( -- | @since 2.3
      Prelude.Show
    , -- | @since 2.3
      Generic
    )

mkRatioSchema ::
  forall (numerator :: Symbol) (denominator :: Symbol).
  Integer ->
  Integer ->
  RatioSchema (numerator ':%: denominator)
mkRatioSchema num denom = RatioSchema (num PlutusTx.Prelude.% denom)

-- | @since 2.3
instance
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  ToJSON (RatioSchema (numerator ':%: denominator))
  where
  toJSON :: RatioSchema (numerator ':%: denominator) -> Value
  toJSON (RatioSchema ratio) =
    object
      [ (jsonFieldSym @numerator, toJSON @Integer $ Ratio.numerator ratio)
      , (jsonFieldSym @denominator, toJSON @Integer $ Ratio.denominator ratio)
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
        <$> obj .: jsonFieldSym @numerator
        <*> obj .: jsonFieldSym @denominator

-- | @since 2.3
instance
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  ToSchema (RatioSchema (numerator ':%: denominator))
  where
  toSchema :: FormSchema
  toSchema = ratioFormSchema @numerator @denominator

-- | @since 2.3
instance
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  ToArgument (RatioSchema (numerator ':%: denominator))
  where
  toArgument (RatioSchema ratio) =
    ratioFixFormArgument @numerator @denominator num denom
    where
      num :: Integer
      num = Ratio.numerator ratio
      denom :: Integer
      denom = Ratio.denominator ratio

-- | @since 2.3
instance
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  OpenApi.ToSchema (RatioSchema (numerator ':%: denominator))
  where
  declareNamedSchema _ =
    ratioDeclareNamedSchema @numerator @denominator "RatioSchema"

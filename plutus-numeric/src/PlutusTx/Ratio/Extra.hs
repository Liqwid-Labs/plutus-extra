module PlutusTx.Ratio.Extra (
  RatioSchema (..),
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
import Prelude

{- | Newtype for deriving Schema and JSON instances

 @since 1.2
-}
newtype RatioSchema (dir :: RatioDirection)
  = RatioSchema PlutusTx.Prelude.Rational
  deriving stock (Prelude.Show, Generic)

mkRatioSchema ::
  forall (from :: Symbol) (to :: Symbol).
  Integer ->
  Integer ->
  RatioSchema (from ':-> to)
mkRatioSchema from to = RatioSchema (to PlutusTx.Prelude.% from)

-- | @since 1.2
instance
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol to
  , KnownSymbol from
  ) =>
  ToJSON (RatioSchema (from ':-> to))
  where
  toJSON :: RatioSchema (from ':-> to) -> Value
  toJSON (RatioSchema ratio) =
    object
      [ (jsonFieldSym @from, toJSON @Integer $ Ratio.denominator ratio)
      , (jsonFieldSym @to, toJSON @Integer $ Ratio.numerator ratio)
      ]

-- | @since 1.2
instance
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol to
  , KnownSymbol from
  ) =>
  FromJSON (RatioSchema (from ':-> to))
  where
  parseJSON :: Value -> Parser (RatioSchema (from ':-> to))
  parseJSON =
    withObject (ratioTypeName @from @to "Ratio") $ \obj ->
      mkRatioSchema
        <$> obj .: jsonFieldSym @from
        <*> obj .: jsonFieldSym @to

-- | @since 1.2
instance
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol from
  , KnownSymbol to
  ) =>
  ToSchema (RatioSchema (from ':-> to))
  where
  toSchema :: FormSchema
  toSchema = ratioFormSchema @from @to

-- | @since 1.2
instance
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol from
  , KnownSymbol to
  ) =>
  ToArgument (RatioSchema (from ':-> to))
  where
  toArgument (RatioSchema ratio) =
    ratioFixFormArgument @from @to fromVal toVal
    where
      fromVal :: Integer
      fromVal = Ratio.denominator ratio
      toVal :: Integer
      toVal = Ratio.numerator ratio

-- | @since 1.2
instance
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol from
  , KnownSymbol to
  ) =>
  OpenApi.ToSchema (RatioSchema (from ':-> to))
  where
  declareNamedSchema _ =
    ratioDeclareNamedSchema @from @to "RatioSchema"

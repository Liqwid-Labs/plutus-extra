{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Module: PlutusTx.SchemaUtils
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Sergey Kurgak <sergey@mlabs.city>
 Portability: GHC only
 Stability: Experimental
-}
module PlutusTx.SchemaUtils (
  RatioFields ((:%:)),
  RatioSchema (..),
  ratioDeclareNamedSchema,
  ratioFixFormArgument,
  ratioFormSchema,
  ratioTypeName,
  jsonFieldSym,
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
import Data.Functor.Foldable (Fix (Fix))
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Declare (type Declare)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Text qualified as Text
import GHC.Exts (fromList)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import PlutusTx.Ratio (Rational)
import PlutusTx.Ratio qualified as Ratio
import Schema (
  FormArgumentF (FormObjectF),
  FormSchema (FormSchemaObject),
  ToArgument (toArgument),
  ToSchema (toSchema),
 )
import Prelude hiding (Rational)

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
      [ (jsonFieldSym @numerator, toJSON @Integer $ Ratio.numerator r)
      , (jsonFieldSym @denominator, toJSON @Integer $ Ratio.denominator r)
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
  OpenApi.ToSchema (RatioSchema (numerator ':%: denominator))
  where
  declareNamedSchema _ =
    ratioDeclareNamedSchema @numerator @denominator "RatioSchema"

-- | @since 2.3
instance
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  Schema.ToSchema (RatioSchema (numerator ':%: denominator))
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
  toArgument (RatioSchema r) =
    ratioFixFormArgument @numerator @denominator num denom
    where
      num :: Integer
      num = Ratio.numerator r
      denom :: Integer
      denom = Ratio.denominator r

{- | Type-level data representing the "direction" that a ratio converts to or from.

 @since 2.3
-}
data RatioFields = Symbol :%: Symbol

-- | @since 2.3
jsonFieldSym :: forall (s :: Symbol). (KnownSymbol s) => Text
jsonFieldSym = pack $ symbolVal (Proxy @s)

-- | @since 2.3
ratioDeclareNamedSchema ::
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  Text ->
  Declare (OpenApi.Definitions OpenApi.Schema) OpenApi.NamedSchema
ratioDeclareNamedSchema name = do
  integerSchema <- OpenApi.declareSchemaRef (Proxy :: Proxy Integer)
  pure $
    OpenApi.NamedSchema (Just name) $
      Prelude.mempty
        { OpenApi._schemaType = Just OpenApi.OpenApiObject
        , OpenApi._schemaProperties =
            fromList
              [ (Text.pack $ symbolVal (Proxy @numerator), integerSchema)
              , (Text.pack $ symbolVal (Proxy @denominator), integerSchema)
              ]
        }

-- | @since 2.3
ratioFixFormArgument ::
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  Integer ->
  Integer ->
  Fix FormArgumentF
ratioFixFormArgument num denom =
  Fix $
    FormObjectF
      [ (symbolVal (Proxy @numerator), toArgument num)
      , (symbolVal (Proxy @denominator), toArgument denom)
      ]

-- | @since 2.3
ratioTypeName ::
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  Prelude.String ->
  Prelude.String
ratioTypeName ratioName =
  Prelude.mconcat
    [ ratioName
    , " "
    , symbolVal (Proxy @numerator)
    , " : "
    , symbolVal (Proxy @denominator)
    ]

-- | @since 2.3
ratioFormSchema ::
  forall (numerator :: Symbol) (denominator :: Symbol).
  ( KnownSymbol numerator
  , KnownSymbol denominator
  ) =>
  FormSchema
ratioFormSchema =
  FormSchemaObject
    [ (symbolVal (Proxy @numerator), toSchema @Integer)
    , (symbolVal (Proxy @denominator), toSchema @Integer)
    ]

-- Helpers

mkRatioSchema ::
  forall (numerator :: Symbol) (denominator :: Symbol).
  Integer ->
  Integer ->
  RatioSchema (numerator ':%: denominator)
mkRatioSchema num = RatioSchema . Ratio.unsafeRatio num

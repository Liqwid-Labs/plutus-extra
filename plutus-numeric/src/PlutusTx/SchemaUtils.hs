{-# LANGUAGE AllowAmbiguousTypes #-}

module PlutusTx.SchemaUtils (
  RatioFields ((:%:)),
  ratioDeclareNamedSchema,
  ratioFixFormArgument,
  ratioFormSchema,
  ratioTypeName,
  jsonFieldSym,
) where

import Data.Functor.Foldable (Fix (Fix))
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Declare (type Declare)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import Data.Text qualified as Text
import GHC.Exts
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Schema (
  FormArgumentF (FormObjectF),
  FormSchema (FormSchemaObject),
  toArgument,
  toSchema,
 )
import Prelude

{- | Type-level data representing the "direction" that a ratio converts to or from.

 @since 1.3
-}
data RatioFields = Symbol :%: Symbol

-- | @since 1.3
jsonFieldSym :: forall (s :: Symbol). (KnownSymbol s) => Text
jsonFieldSym = pack $ symbolVal (Proxy @s)

-- | @since 1.3
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

-- | @since 1.3
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

-- | @since 1.3
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

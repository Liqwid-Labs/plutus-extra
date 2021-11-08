{-# LANGUAGE AllowAmbiguousTypes #-}

module PlutusTx.SchemaUtils (
  RatioDirection (..),
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

 @since 1.2
-}
data RatioDirection = Symbol :-> Symbol

-- | @since 1.2
jsonFieldSym :: forall (s :: Symbol). (KnownSymbol s) => Text
jsonFieldSym = pack $ symbolVal (Proxy @s)

-- | @since 1.2
ratioDeclareNamedSchema ::
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol from
  , KnownSymbol to
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
              [ (Text.pack $ symbolVal (Proxy @from), integerSchema)
              , (Text.pack $ symbolVal (Proxy @to), integerSchema)
              ]
        }

-- | @since 1.2
ratioFixFormArgument ::
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol from
  , KnownSymbol to
  ) =>
  Integer ->
  Integer ->
  Fix FormArgumentF
ratioFixFormArgument fromVal toVal =
  Fix $
    FormObjectF
      [ (symbolVal (Proxy @from), toArgument fromVal)
      , (symbolVal (Proxy @to), toArgument toVal)
      ]

-- | @since 1.2
ratioFormSchema ::
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol from
  , KnownSymbol to
  ) =>
  FormSchema
ratioFormSchema =
  FormSchemaObject
    [ (symbolVal (Proxy @from), toSchema @Integer)
    , (symbolVal (Proxy @to), toSchema @Integer)
    ]

ratioTypeName ::
  forall (from :: Symbol) (to :: Symbol).
  ( KnownSymbol from
  , KnownSymbol to
  ) =>
  Prelude.String ->
  Prelude.String
ratioTypeName ratioName =
  Prelude.mconcat
    [ ratioName
    , " "
    , symbolVal (Proxy @from)
    , " : "
    , symbolVal (Proxy @to)
    ]

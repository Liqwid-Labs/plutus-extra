{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}

module PlutusTx.Skeleton.Builder (build, renderSkeleton) where

import Data.Kind (Type)
import PlutusTx.Prelude
import PlutusTx.Skeleton.Internal (
  Skeleton (
    BoolS,
    ConS,
    IntegerS,
    ListS,
    RecS,
    StringS,
    TupleS
  ),
 )
import PlutusTx.Skeleton.String (intToString)

newtype Builder = Builder (BuiltinString -> BuiltinString)

instance Semigroup Builder where
  {-# INLINEABLE (<>) #-}
  Builder f <> Builder g = Builder (g . f)

instance Monoid Builder where
  {-# INLINEABLE mempty #-}
  mempty = Builder id

{-# INLINEABLE build #-}
build :: Builder -> BuiltinString
build (Builder f) = f ""

{-# INLINEABLE renderSkeleton #-}
renderSkeleton :: Skeleton -> Builder
renderSkeleton = \case
  BoolS b -> embed (if b then "true" else "false")
  IntegerS i -> embed (intToString i)
  StringS bis -> quotes (embed bis)
  ConS conName conArgs -> renderTagged conName conArgs
  RecS recConName fieldVals -> renderKV recConName fieldVals
  TupleS x y z -> renderTuple x y z
  ListS xs -> renderArray renderSkeleton xs

{-# INLINEABLE renderTagged #-}
renderTagged :: BuiltinString -> [Skeleton] -> Builder
renderTagged name args =
  renderObject
    id
    [ ("tag", quotes (embed name))
    , ("arguments", renderArray renderSkeleton args)
    ]

{-# INLINEABLE renderKV #-}
renderKV :: BuiltinString -> [(BuiltinString, Skeleton)] -> Builder
renderKV name fieldVals =
  renderObject
    id
    [ ("recordTag", quotes (embed name))
    , ("fields", renderObject renderSkeleton fieldVals)
    ]

{-# INLINEABLE renderTuple #-}
renderTuple :: Skeleton -> Skeleton -> Maybe Skeleton -> Builder
renderTuple x y =
  renderObject renderSkeleton . \case
    Nothing -> [("fst", x), ("snd", y)]
    Just z -> [("fst", x), ("snd", y), ("thd", z)]

{-# INLINEABLE renderArray #-}
renderArray :: forall (a :: Type). (a -> Builder) -> [a] -> Builder
renderArray f = wrap ("[", "]") . commaSep f

{-# INLINEABLE renderObject #-}
renderObject :: forall (a :: Type). (a -> Builder) -> [(BuiltinString, a)] -> Builder
renderObject f = wrap ("{", "}") . commaSep go
  where
    go :: (BuiltinString, a) -> Builder
    go (k, v) = (quotes (embed k) <> embed ":") <+> f v

-- Comma separated
{-# INLINEABLE commaSep #-}
commaSep :: forall (a :: Type). (a -> Builder) -> [a] -> Builder
commaSep f = go
  where
    go :: [a] -> Builder
    go = \case
      [] -> mempty
      [x] -> f x
      (x : xs) -> f x <> embed ", " <> go xs

-- Space
{-# INLINEABLE (<+>) #-}
(<+>) :: Builder -> Builder -> Builder
xs <+> ys = xs <> embed " " <> ys

infixl 6 <+>

-- Wrap
{-# INLINEABLE wrap #-}
wrap :: (BuiltinString, BuiltinString) -> Builder -> Builder
wrap (l, r) b = embed l <> b <> embed r

-- String quotes
{-# INLINEABLE quotes #-}
quotes :: Builder -> Builder
quotes = wrap ("\"", "\"")

-- Literal
{-# INLINEABLE embed #-}
embed :: BuiltinString -> Builder
embed s = Builder (<> s)

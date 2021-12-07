{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}

module PlutusTx.Skeleton.Builder (build, renderSkeleton) where

import PlutusTx.Prelude
import PlutusTx.Skeleton.Internal (
  Skeleton (
    AssocMapS,
    BoolS,
    ByteStringS,
    ConS,
    IntegerS,
    ListS,
    RecS,
    StringS,
    TupleS
  ),
 )
import PlutusTx.Skeleton.String (bsToString, intToString)

newtype Builder = Builder [BuiltinString]
  deriving (Semigroup, Monoid) via [BuiltinString]

{-# INLINEABLE build #-}
build :: Builder -> BuiltinString
build (Builder xs) = foldr go "" xs
  where
    go :: BuiltinString -> BuiltinString -> BuiltinString
    go x acc = x <> acc

{-# INLINEABLE renderSkeleton #-}
renderSkeleton :: Skeleton -> Builder
renderSkeleton = \case
  ByteStringS bibs ->
    embed "BuiltinByteString:" <+> embed (bsToString bibs)
  BoolS b -> embed (if b then "True" else "False")
  IntegerS i -> embed (intToString i)
  StringS bis -> embed ("\"" <> bis <> "\"")
  AssocMapS aMap -> embed "AssocMap:" <\> foldMap renderKeyVal aMap
  ConS conName conArgs -> embed (conName <> ":") <\> foldMap renderConArg conArgs
  RecS recConName fieldVal fieldVals ->
    embed ("Record " <> recConName <> ":")
      <\> foldr renderFieldVals (renderFieldVal fieldVal) fieldVals
  TupleS x xs -> embed "Tuple:" <+> foldr renderTuple (renderSkeleton x) xs
  ListS xs -> embed "[" <\> foldMap renderListItem xs <\> embed "]"

{-# INLINEABLE renderKeyVal #-}
renderKeyVal :: (Skeleton, Skeleton) -> Builder
renderKeyVal (k, v) =
  embed "\n, " <> renderSkeleton k <> embed " -> " <> renderSkeleton v

{-# INLINEABLE renderConArg #-}
renderConArg :: Skeleton -> Builder
renderConArg arg = embed "\n, " <> renderSkeleton arg

{-# INLINEABLE renderFieldVal #-}
renderFieldVal :: (BuiltinString, Skeleton) -> Builder
renderFieldVal (name, val) =
  embed ("\n, " <> name <> ":") <+> renderSkeleton val

{-# INLINEABLE renderFieldVals #-}
renderFieldVals :: (BuiltinString, Skeleton) -> Builder -> Builder
renderFieldVals x acc = acc <> renderFieldVal x

{-# INLINEABLE renderTuple #-}
renderTuple :: Skeleton -> Builder -> Builder
renderTuple sk acc = (acc <> embed ",") <+> renderSkeleton sk

{-# INLINEABLE renderListItem #-}
renderListItem :: Skeleton -> Builder
renderListItem sk = renderSkeleton sk <> embed ", "

-- Space
{-# INLINEABLE (<+>) #-}
(<+>) :: Builder -> Builder -> Builder
xs <+> ys = xs <> embed " " <> ys

infixl 6 <+>

-- Newline
{-# INLINEABLE (<\>) #-}
(<\>) :: Builder -> Builder -> Builder
xs <\> ys = xs <> embed "\n" <> ys

infixl 6 <\>

{-# INLINEABLE embed #-}
embed :: BuiltinString -> Builder
embed s = Builder [s]

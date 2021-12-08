{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}

module PlutusTx.Skeleton.Builder (build, renderSkeleton) where

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
  BoolS b -> embed (if b then "True" else "False")
  IntegerS i -> embed (intToString i)
  StringS bis -> embed ("\"" <> bis <> "\"")
  ConS conName conArgs -> embed (conName <> ":") <\> foldMap renderConArg conArgs
  RecS recConName fieldVals ->
    embed ("Record " <> recConName <> ":") <\> foldMap renderFieldVal fieldVals
  TupleS x y -> embed "Tuple:" <+> (renderSkeleton x <> embed ", " <> renderSkeleton y)
  ListS xs -> embed "[" <\> foldMap renderListItem xs <\> embed "]"

{-# INLINEABLE renderConArg #-}
renderConArg :: Skeleton -> Builder
renderConArg arg = embed "\n, " <> renderSkeleton arg

{-# INLINEABLE renderFieldVal #-}
renderFieldVal :: (BuiltinString, Skeleton) -> Builder
renderFieldVal (name, val) =
  embed ("\n, " <> name <> ":") <+> renderSkeleton val

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

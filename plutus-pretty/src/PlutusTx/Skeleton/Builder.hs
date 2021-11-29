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

-- Lines tagged with their indentation. Uses an endomorphism-based
-- representation.
newtype Builder = Builder ([(Integer, BuiltinString)] -> [(Integer, BuiltinString)])

instance Semigroup Builder where
  {-# INLINEABLE (<>) #-}
  Builder f <> Builder g = Builder (f . g)

instance Monoid Builder where
  {-# INLINEABLE mempty #-}
  mempty = Builder id

{-# INLINEABLE build #-}
build :: Builder -> BuiltinString
build (Builder f) = intersperse "\n" . fmap render . f $ []

{-# INLINEABLE renderSkeleton #-}
renderSkeleton :: Skeleton -> Builder
renderSkeleton = renderRec 0

-- Helpers

{-# INLINEABLE renderRec #-}
renderRec :: Integer -> Skeleton -> Builder
renderRec indent = \case
  ByteStringS bibs ->
    embed indent "BuiltinByteString:"
      <> embed (increase indent) (bsToString bibs)
  BoolS b ->
    embed indent ("Bool: " <> if b then "True" else "False")
  IntegerS i ->
    embed indent ("Integer: " <> intToString i)
  StringS bis ->
    embed indent "BuiltinString:"
      <> embed (increase indent) bis
  AssocMapS aMap ->
    embed indent "AssocMap:"
      <> foldMap (renderKeyVal (increase indent)) aMap
  ConS conName conArgs ->
    embed indent conName
      <> foldMap (renderRec (increase indent)) conArgs
  RecS recConName fieldVal fieldVals ->
    embed indent ("Record " <> recConName <> ":")
      <> renderFieldVal (increase indent) fieldVal
      <> foldMap (renderFieldVal (increase indent)) fieldVals
  TupleS x xs ->
    embed indent "Tuple:"
      <> renderRec (increase indent) x
      <> foldMap (renderRec (increase indent)) xs
  ListS xs ->
    embed indent "List:"
      <> foldMap (renderRec (increase indent)) xs

{-# INLINEABLE renderKeyVal #-}
renderKeyVal :: Integer -> (Skeleton, Skeleton) -> Builder
renderKeyVal indent (k, v) =
  embed indent "Key:"
    <> renderRec (increase indent) k
    <> embed indent "Value:"
    <> renderRec (increase indent) v

{-# INLINEABLE renderFieldVal #-}
renderFieldVal :: Integer -> (BuiltinString, Skeleton) -> Builder
renderFieldVal indent (field, val) =
  embed indent (field <> ":")
    <> renderRec (increase indent) val

{-# INLINEABLE increase #-}
increase :: Integer -> Integer
increase = (+ 2)

{-# INLINEABLE embed #-}
embed :: Integer -> BuiltinString -> Builder
embed indent s = Builder . (:) $ (indent, s)

-- Adds indentation according to annotation.
{-# INLINEABLE render #-}
render :: (Integer, BuiltinString) -> BuiltinString
render (indent, s) = repeat indent s <> s

-- Links together every element of the list with the given linking string.
{-# INLINEABLE intersperse #-}
intersperse :: BuiltinString -> [BuiltinString] -> BuiltinString
intersperse sep = \case
  [] -> ""
  [s] -> s
  (s : ss) -> s <> sep <> intersperse sep ss

-- Links together n copies of argument
{-# INLINEABLE repeat #-}
repeat :: Integer -> BuiltinString -> BuiltinString
repeat i s
  | i <= zero = ""
  | otherwise = s <> repeat (i - one) s

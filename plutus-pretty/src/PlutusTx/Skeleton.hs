module PlutusTx.Skeleton (
  -- * Type and type class
  Skeleton,
  Skeletal (skeletize),

  -- * TH helper
  makeSkeletal,

  -- * Functions
  showSkeleton,
  traceSkeleton,
  traceErrorSkeleton,
  traceIfFalseSkeleton,
  traceIfTrueSkeleton,
) where

import Data.Kind (Type)
import PlutusTx.Prelude qualified as PTx
import PlutusTx.Skeleton.Internal (Skeletal (skeletize), Skeleton)
import PlutusTx.Skeleton.QQ (makeSkeletal)

-- | @since 2.1
showSkeleton ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  [PTx.BuiltinString]
showSkeleton = _

-- | @since 2.1
{-# INLINEABLE traceSkeleton #-}
traceSkeleton ::
  forall (b :: Type) (a :: Type).
  (Skeletal a) =>
  a ->
  b ->
  b
traceSkeleton x = foldTrace (showSkeleton x)

-- | @since 2.1
{-# INLINEABLE traceErrorSkeleton #-}
traceErrorSkeleton ::
  forall (b :: Type) (a :: Type).
  (Skeletal a) =>
  a ->
  b
traceErrorSkeleton x = foldTraceError (showSkeleton x)

-- | @since 2.1
{-# INLINEABLE traceIfFalseSkeleton #-}
traceIfFalseSkeleton ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  Bool ->
  Bool
traceIfFalseSkeleton x b =
  if b then True else foldTrace (showSkeleton x) False

-- | @since 2.1
{-# INLINEABLE traceIfTrueSkeleton #-}
traceIfTrueSkeleton ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  Bool ->
  Bool
traceIfTrueSkeleton x b =
  if b then foldTrace (showSkeleton x) True else False

-- Helpers

foldTrace :: forall (a :: Type). [PTx.BuiltinString] -> a -> a
foldTrace = PTx.foldr go PTx.id
  where
    go :: PTx.BuiltinString -> (a -> a) -> (a -> a)
    go bbs acc = PTx.trace bbs acc

foldTraceError :: forall (a :: Type). [PTx.BuiltinString] -> a
foldTraceError bs = PTx.error . foldTrace bs $ ()

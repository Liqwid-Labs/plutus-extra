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
  PTx.BuiltinString
showSkeleton = _

-- | @since 2.1
{-# INLINEABLE traceSkeleton #-}
traceSkeleton ::
  forall (b :: Type) (a :: Type).
  (Skeletal a) =>
  a ->
  b ->
  b
traceSkeleton x = PTx.trace (showSkeleton x)

-- | @since 2.1
{-# INLINEABLE traceErrorSkeleton #-}
traceErrorSkeleton ::
  forall (b :: Type) (a :: Type).
  (Skeletal a) =>
  a ->
  b
traceErrorSkeleton x = PTx.traceError (showSkeleton x)

-- | @since 2.1
{-# INLINEABLE traceIfFalseSkeleton #-}
traceIfFalseSkeleton ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  Bool ->
  Bool
traceIfFalseSkeleton x = PTx.traceIfFalse (showSkeleton x)

-- | @since 2.1
{-# INLINEABLE traceIfTrueSkeleton #-}
traceIfTrueSkeleton ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  Bool ->
  Bool
traceIfTrueSkeleton x = PTx.traceIfTrue (showSkeleton x)

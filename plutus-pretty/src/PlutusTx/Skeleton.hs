{-# LANGUAGE NoImplicitPrelude #-}

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
import PlutusTx.Prelude
import PlutusTx.Skeleton.Builder (build, renderSkeleton)
import PlutusTx.Skeleton.Internal (Skeletal (skeletize), Skeleton)
import PlutusTx.Skeleton.QQ (makeSkeletal)

-- | @since 2.1
{-# INLINEABLE showSkeleton #-}
showSkeleton ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  BuiltinString
showSkeleton = build . renderSkeleton . skeletize

-- | @since 2.1
{-# INLINEABLE traceSkeleton #-}
traceSkeleton ::
  forall (b :: Type) (a :: Type).
  (Skeletal a) =>
  a ->
  b ->
  b
traceSkeleton x = trace (showSkeleton x)

-- | @since 2.1
{-# INLINEABLE traceErrorSkeleton #-}
traceErrorSkeleton ::
  forall (b :: Type) (a :: Type).
  (Skeletal a) =>
  a ->
  b
traceErrorSkeleton x = traceError (showSkeleton x)

-- | @since 2.1
{-# INLINEABLE traceIfFalseSkeleton #-}
traceIfFalseSkeleton ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  Bool ->
  Bool
traceIfFalseSkeleton x = traceIfFalse (showSkeleton x)

-- | @since 2.1
{-# INLINEABLE traceIfTrueSkeleton #-}
traceIfTrueSkeleton ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  Bool ->
  Bool
traceIfTrueSkeleton x = traceIfTrue (showSkeleton x)

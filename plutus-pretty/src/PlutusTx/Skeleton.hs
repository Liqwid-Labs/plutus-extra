{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- | Module: PlutusTx.Skeleton
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A type class for constructing structural representations of types on-chain,
 and some pretty-printing utilities.
-}
module PlutusTx.Skeleton (
  -- * Type and type class
  Skeleton,
  Skeletal (skeletize),

  -- * TH helper
  makeSkeletal,

  -- * Functions
  showSkeletal,
  traceSkeletal,
  traceErrorSkeletal,
  traceIfFalseSkeletal,
  traceIfTrueSkeletal,
) where

import Data.Kind (Type)
import PlutusTx.Prelude
import PlutusTx.Skeleton.Builder (build, renderSkeleton)
import PlutusTx.Skeleton.Internal (Skeletal (skeletize), Skeleton)
import PlutusTx.Skeleton.QQ (makeSkeletal)

{- | Constructs a prettyprinted 'BuiltinString' representation of any type with
 a 'Skeletal' instance.

 @since 2.1
-}
{-# INLINEABLE showSkeletal #-}
showSkeletal ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  BuiltinString
showSkeletal = build . renderSkeleton . skeletize

{- | As 'trace', but for any 'Skeletal'.

 @since 2.1
-}
{-# INLINEABLE traceSkeletal #-}
traceSkeletal ::
  forall (b :: Type) (a :: Type).
  (Skeletal a) =>
  a ->
  b ->
  b
traceSkeletal x = trace (showSkeletal x)

{- | As 'traceError', but for any 'Skeletal'.

 @since 2.1
-}
{-# INLINEABLE traceErrorSkeletal #-}
traceErrorSkeletal ::
  forall (b :: Type) (a :: Type).
  (Skeletal a) =>
  a ->
  b
traceErrorSkeletal x = traceError (showSkeletal x)

{- | As 'traceIfFalse', but for any 'Skeletal'.

 @since 2.1
-}
{-# INLINEABLE traceIfFalseSkeletal #-}
traceIfFalseSkeletal ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  Bool ->
  Bool
traceIfFalseSkeletal x = traceIfFalse (showSkeletal x)

{- | As 'traceIfTrue', but for any 'Skeletal'.

 @since 2.1
-}
{-# INLINEABLE traceIfTrueSkeletal #-}
traceIfTrueSkeletal ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  Bool ->
  Bool
traceIfTrueSkeletal x = traceIfTrue (showSkeletal x)

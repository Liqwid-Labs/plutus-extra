{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}

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

 = Representation specifics

 We use a JSON representation of the 'Skeleton' of @a@. More precisely:

 * 'Bool's are represented as a JSON boolean.
 * 'Integer's are represented as JSON numbers.
 * 'BuiltinString's are represented as JSON strings.
 * 'BuiltinByteString's are represented as JSON strings, with each byte
   represented as an escaped numerical code.
 * ADTs which are records are represented as tagged JSON objects (see below).
 * ADTS which are /not/ records are represented as a different kind of
   tagged JSON object (see below).
 * Tuples are represented as a two-item object (see below).
 * Lists are represented as JSON arrays of the representations of the
   'Skeleton's of the list's values.

 For record ADTs, we use JSON objects with the following structure:

 * A \'recordTag\' field, storing the name of the record value's constructor
   as a JSON string.
 * A \'fields\' field, storing a JSON object whose keys are the record field
   names, and whose values are the JSON representations of the 'Skeleton's of
   the corresponding record field values.

 For non-record ADTs, we use JSON objects with the following structure:

 * A \'tag\' field, storing the name of the data constructor of the value as a
   JSON string.
 * An \'arguments\' field, storing a JSON array of the JSON representations of
   all the constructor arguments.

 For tuples, we use JSON objects with the following structure:

 * A \'fst\' field, storing the JSON representation of the tuple's first
   element.
 * A \'snd\' field, storing the JSON representation of the tuple's second
   element.

 @since 2.1
-}
{-# INLINEABLE showSkeletal #-}
showSkeletal ::
  forall (a :: Type).
  (Skeletal a) =>
  a ->
  BuiltinString
showSkeletal = build . renderSkeleton . skeletize

{- | As 'trace', but for any 'Skeletal'. Will trace the representation emitted
 by 'showSkeletal'.

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

{- | As 'traceError', but for any 'Skeletal'. Will trace the representation
 emitted by 'showSkeletal'.

 @since 2.1
-}
{-# INLINEABLE traceErrorSkeletal #-}
traceErrorSkeletal ::
  forall (b :: Type) (a :: Type).
  (Skeletal a) =>
  a ->
  b
traceErrorSkeletal x = traceError (showSkeletal x)

{- | As 'traceIfFalse', but for any 'Skeletal'. Will trace the representation
 emitted by 'showSkeletal'.

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

{- | As 'traceIfTrue', but for any 'Skeletal'. Will trace the representation
 emitted by 'showSkeletal'.

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

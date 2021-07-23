{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusTx.Error.Extra (
  eitherError,
  maybeError,
) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import PlutusTx.Builtins qualified as PlutusTx
import PlutusTx.Prelude (Either (Left, Right), Maybe (Just, Nothing), traceError)

--------------------------------------------------------------------------------

{-# INLINEABLE eitherError #-}

-- | Throw a PlutusTx runtime error from a `Left`
eitherError :: forall (a :: Type). Either PlutusTx.String a -> a
eitherError (Left err) = traceError err
eitherError (Right r) = r

{-# INLINEABLE maybeError #-}

-- | Throw a PlutusTx runtime error from a `Nothing`, using the provided error message
maybeError :: forall (a :: Type). PlutusTx.String -> Maybe a -> a
maybeError err Nothing = traceError err
maybeError _ (Just x) = x

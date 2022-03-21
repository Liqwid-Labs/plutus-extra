{-# LANGUAGE RoleAnnotations #-}

module PlutusTx.AsData.Internal (AsData (AsData)) where

import Data.Kind (Type)
import PlutusTx (BuiltinData)

type role AsData representational
newtype AsData (x :: Type) = AsData BuiltinData

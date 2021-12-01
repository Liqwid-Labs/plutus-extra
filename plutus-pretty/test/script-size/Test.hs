{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Test (Foo (..)) where

import Plutus.V1.Ledger.Value (Value)
import PlutusTx.Prelude
import PlutusTx.Skeleton (makeSkeletal)

data Foo = Foo
  { bar :: Integer
  , baz :: Value
  , quux :: [BuiltinString]
  }

instance Eq Foo where
  {-# INLINEABLE (==) #-}
  x == y =
    bar x == bar y
      && baz x == baz y
      && quux x == quux y

$(makeSkeletal ''Foo)

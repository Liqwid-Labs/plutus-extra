{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test (Foo) where

import Data.Kind (Type)
import PlutusTx.Deriving (deriveEq)
import PlutusTx.Prelude

data Foo (a :: Type)
  = Empty
  | Mono BuiltinString Integer
  | Poly a
  | Recursive BuiltinString (Foo a)

$(deriveEq ''Foo)

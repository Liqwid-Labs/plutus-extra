{-# LANGUAGE Trustworthy #-}

{- |
 Module: Text.Show.Pretty.Extra
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Some helper type classes for \'lifting\' prettyprinting to various
 higher-rank types.
-}
module Text.Show.Pretty.Extra (
  PrettyVal1 (..),
  PrettyVal2 (..),
) where

import Data.Kind (Type)
import Plutus.V1.Ledger.Interval (
  Extended (Finite, NegInf, PosInf),
  Interval (ivFrom, ivTo),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import PlutusTx.AssocMap qualified as AssocMap
import Text.Show.Pretty (
  Value (Con, List, Rec, Tuple),
  prettyVal,
 )
import Prelude

{- | Indicates the ability to \'lift\' prettyprinting of one type argument.

 @since 1.0
-}
class PrettyVal1 (f :: Type -> Type) where
  liftPrettyVal :: (a -> Value) -> f a -> Value

-- | @since 1.0
instance PrettyVal1 Maybe where
  {-# INLINEABLE liftPrettyVal #-}
  liftPrettyVal f = \case
    Nothing -> Con "Nothing" []
    Just x -> Con "Just" [f x]

-- | @since 1.0
instance PrettyVal1 [] where
  {-# INLINEABLE liftPrettyVal #-}
  liftPrettyVal f = List . fmap f

-- | @since 1.0
instance PrettyVal1 Interval where
  {-# INLINEABLE liftPrettyVal #-}
  liftPrettyVal f inter =
    Rec
      "Interval"
      [ ("ivFrom", liftPrettyVal f . ivFrom $ inter)
      , ("ivTo", liftPrettyVal f . ivTo $ inter)
      ]

-- | @since 1.0
instance PrettyVal1 LowerBound where
  {-# INLINEABLE liftPrettyVal #-}
  liftPrettyVal f (LowerBound ext clos) =
    Con "LowerBound" [liftPrettyVal f ext, prettyVal clos]

-- | @since 1.0
instance PrettyVal1 UpperBound where
  {-# INLINEABLE liftPrettyVal #-}
  liftPrettyVal f (UpperBound ext clos) =
    Con "UpperBound" [liftPrettyVal f ext, prettyVal clos]

-- | @since 1.0
instance PrettyVal1 Extended where
  {-# INLINEABLE liftPrettyVal #-}
  liftPrettyVal f = \case
    NegInf -> Con "NegInf" []
    Finite x -> Con "Finite" [f x]
    PosInf -> Con "PosInf" []

{- | Indicates the ability to \'lift\' prettyprinting of two type arguments.

 @since 1.0
-}
class PrettyVal2 (f :: Type -> Type -> Type) where
  liftPrettyVal2 :: (a -> Value) -> (b -> Value) -> f a b -> Value

-- | @since 1.0
instance PrettyVal2 (,) where
  {-# INLINEABLE liftPrettyVal2 #-}
  liftPrettyVal2 f g (x, y) = Tuple [f x, g y]

-- | @since 1.0
instance PrettyVal2 AssocMap.Map where
  {-# INLINEABLE liftPrettyVal2 #-}
  liftPrettyVal2 f g =
    Con "AssocMap" . (: [])
      . liftPrettyVal (liftPrettyVal2 f g)
      . AssocMap.toList

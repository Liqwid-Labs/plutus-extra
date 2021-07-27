{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

-- | Extra functions for `Plutus.V1.Ledger.Interval`
module Plutus.V1.Ledger.Interval.Extra (width) where

--------------------------------------------------------------------------------

import Data.Kind (Type)

--------------------------------------------------------------------------------

import Plutus.V1.Ledger.Interval
import PlutusTx.Prelude

--------------------------------------------------------------------------------

{-# INLINEABLE width #-}

-- | Number of values covered by the interval, if finite. @width (from x) == Nothing@.
width :: forall (a :: Type). (Enum a) => Interval a -> Maybe Integer
width (Interval (LowerBound (Finite (fromEnum -> x)) in1) (UpperBound (Finite (fromEnum -> y)) in2)) =
  let lowestValue = if in1 then x else x + 1
      highestValue = if in2 then y else y - 1
   in if lowestValue <= highestValue
        then -- +1 avoids fencepost error: width of [2,4] is 3.
          Just $ (highestValue - lowestValue) + 1
        else -- low > high, i.e. empty interval
          Nothing
-- Infinity is involved!
width _ = Nothing

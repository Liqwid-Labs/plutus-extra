{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-specialise -fno-ignore-interface-pragmas -fno-omit-interface-pragmas #-}

-- | Extras for POSIXTime handling
module Plutus.V1.Ledger.Time.Extra (mkTimeRangeWidth) where

--------------------------------------------------------------------------------

import Control.Arrow ((>>>))

--------------------------------------------------------------------------------

import Ledger.TimeSlot qualified as TimeSlot
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Time qualified as Time
import PlutusTx.Prelude

--------------------------------------------------------------------------------

{- | Construct a POSIXTimeRange, from a given `POSIXTime` and a max width,
 rounding to the equivalent `Slot` times which will be passed on-chain.
 See: docs/codebase/POSIXTime.lhs for info
-}
mkTimeRangeWidth ::
  TimeSlot.SlotConfig -> Time.POSIXTime -> Time.POSIXTime -> Time.POSIXTimeRange
mkTimeRangeWidth
  cfg@TimeSlot.SlotConfig {scSlotLength}
  from
  width@(Time.POSIXTime w) =
    Interval.Interval
      (Interval.lowerBound $ roundSlotStart cfg from)
      (Interval.strictUpperBound end)
    where
      end :: Time.POSIXTime
      end =
        if w <= scSlotLength
          then roundSlotEnd cfg from
          else roundSlotStart cfg (from + width) - 1

-- | Round POSIXTime to the start of the enclosing slot
roundSlotStart :: TimeSlot.SlotConfig -> Time.POSIXTime -> Time.POSIXTime
roundSlotStart cfg =
  TimeSlot.posixTimeToEnclosingSlot cfg >>> TimeSlot.slotToBeginPOSIXTime cfg

-- | Round POSIXTime to the end of the enclosing slot
roundSlotEnd :: TimeSlot.SlotConfig -> Time.POSIXTime -> Time.POSIXTime
roundSlotEnd cfg =
  TimeSlot.posixTimeToEnclosingSlot cfg >>> TimeSlot.slotToEndPOSIXTime cfg

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas -fno-specialize #-}

-- These definitions aren't useful in themselves; they're only designed to make
-- sure that our code compiles on-chain.
--
-- Koz
module Compilation (
  testNatMonus,
  testNatRatioMonus,
  testNatDivModOne,
  testNatDiv,
  testIntegerDiv,
  testNatRem,
  testIntegerRem,
  testNatPowNat,
  testNatRatioPowNat,
  testIntegerPowNat,
  testRationalPowNat,
  testNatRatioPowInteger,
  testRationalPowInteger,
  testNatRatioReciprocal,
  testRationalReciprocal,
  testNatRatioDivision,
  testRationalDivision,
  testNatAddExtend,
  testNatRatioAddExtend,
  testIntegerAbs,
  testRationalAbs,
  testIntegerProjectAbs,
  testRationalProjectAbs,
  testIntegerRestrictMay,
  testRationalRestrictMay,
  testIntegerSignum,
  testRationalSignum,
  testNaturalParity,
  testNatRatioFromNatural,
  testNatRatioNatRatio,
  testNatRatioNumerator,
  testNatRatioDenominator,
  testNatRatioTruncate,
  testNatRatioCeiling,
  testNatRatioRound,
  testNatRatioProperFraction,
) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.NatRatio (NatRatio, frac)
import PlutusTx.NatRatio qualified as NatRatio
import PlutusTx.Natural (Natural, Parity, nat, parity)
import PlutusTx.Numeric ()
import PlutusTx.Numeric.Extra
import PlutusTx.Prelude hiding (divMod, take)
import PlutusTx.TH (compile)
import Prelude ()

{-# INLINEABLE natNumber #-}
natNumber :: Natural
natNumber = [nat| 10 |]

{-# INLINEABLE natLst #-}
natLst :: [Natural]
natLst = [zero, one, natNumber]

{-# INLINEABLE natRatioNumber #-}
natRatioNumber :: NatRatio
natRatioNumber = [frac| (10,3) |]

{-# INLINEABLE natRatioLst #-}
natRatioLst :: [NatRatio]
natRatioLst = [zero, one, natRatioNumber]

{-# INLINEABLE ratioNumber #-}
ratioNumber :: Rational
ratioNumber = 10 % 3

{-# INLINEABLE ratioLst #-}
ratioLst :: [Rational]
ratioLst = [zero, one, ratioNumber, negate ratioNumber]

{-# INLINEABLE integerNumber #-}
integerNumber :: Integer
integerNumber = 10

{-# INLINEABLE integerLst #-}
integerLst :: [Integer]
integerLst = [zero, one, -10, 10]

{-# INLINEABLE testNatMonus #-}
testNatMonus :: CompiledCode [Natural]
testNatMonus =
  $$( compile
        [||
        fmap (natNumber `monus`) natLst
          ++ fmap (natNumber ^-) natLst
        ||]
    )

{-# INLINEABLE testNatRatioMonus #-}
testNatRatioMonus :: CompiledCode [NatRatio]
testNatRatioMonus =
  $$( compile
        [||
        fmap (natRatioNumber `monus`) natRatioLst
          ++ fmap (natRatioNumber ^-) natRatioLst
        ||]
    )

{-# INLINEABLE testNatDivModOne #-}
testNatDivModOne :: CompiledCode [(Natural, Natural)]
testNatDivModOne =
  $$(compile [||fmap (natNumber `divMod`) natLst||])

{-# INLINEABLE testIntegerDivModOne #-}
testIntegerDivModOne :: CompiledCode [(Integer, Integer)]
testIntegerDivModOne =
  $$(compile [||fmap (integerNumber `divMod`) integerLst||])

{-# INLINEABLE testNatDiv #-}
testNatDiv :: CompiledCode [Natural]
testNatDiv =
  $$(compile [||fmap (natNumber `div`) natLst||])

{-# INLINEABLE testIntegerDiv #-}
testIntegerDiv :: CompiledCode [Integer]
testIntegerDiv =
  $$(compile [||fmap (integerNumber `div`) integerLst||])

{-# INLINEABLE testNatRem #-}
testNatRem :: CompiledCode [Natural]
testNatRem =
  $$(compile [||fmap (natNumber `rem`) natLst||])

{-# INLINEABLE testIntegerRem #-}
testIntegerRem :: CompiledCode [Integer]
testIntegerRem =
  $$(compile [||fmap (integerNumber `rem`) integerLst||])

{-# INLINEABLE testNatPowNat #-}
testNatPowNat :: CompiledCode [Natural]
testNatPowNat =
  $$( compile
        [||
        fmap (natNumber `powNat`) natLst
          ++ fmap (natNumber ^+) natLst
        ||]
    )

{-# INLINEABLE testNatRatioPowNat #-}
testNatRatioPowNat :: CompiledCode [NatRatio]
testNatRatioPowNat =
  $$( compile
        [||
        fmap (natRatioNumber `powNat`) natLst
          ++ fmap (natRatioNumber ^+) natLst
        ||]
    )

{-# INLINEABLE testIntegerPowNat #-}
testIntegerPowNat :: CompiledCode [Integer]
testIntegerPowNat =
  $$( compile
        [||
        fmap (integerNumber `powNat`) natLst
          ++ fmap (integerNumber ^+) natLst
        ||]
    )

{-# INLINEABLE testRationalPowNat #-}
testRationalPowNat :: CompiledCode [Rational]
testRationalPowNat =
  $$( compile
        [||
        fmap (ratioNumber `powNat`) natLst
          ++ fmap (ratioNumber ^+) natLst
        ||]
    )

{-# INLINEABLE testNatRatioPowInteger #-}
testNatRatioPowInteger :: CompiledCode [NatRatio]
testNatRatioPowInteger =
  $$( compile
        [||
        fmap (natRatioNumber `powInteger`) integerLst
          ++ fmap (natRatioNumber ^) integerLst
        ||]
    )

{-# INLINEABLE testRationalPowInteger #-}
testRationalPowInteger :: CompiledCode [Rational]
testRationalPowInteger =
  $$( compile
        [||
        fmap (ratioNumber `powInteger`) integerLst
          ++ fmap (ratioNumber ^) integerLst
        ||]
    )

{-# INLINEABLE testNatRatioReciprocal #-}
testNatRatioReciprocal :: CompiledCode [NatRatio]
testNatRatioReciprocal =
  $$(compile [||fmap reciprocal natRatioLst||])

{-# INLINEABLE testRationalReciprocal #-}
testRationalReciprocal :: CompiledCode [Rational]
testRationalReciprocal =
  $$(compile [||fmap reciprocal ratioLst||])

{-# INLINEABLE testNatRatioDivision #-}
testNatRatioDivision :: CompiledCode [NatRatio]
testNatRatioDivision =
  $$(compile [||fmap (natRatioNumber /) natRatioLst||])

{-# INLINEABLE testRationalDivision #-}
testRationalDivision :: CompiledCode [Rational]
testRationalDivision =
  $$(compile [||fmap (ratioNumber /) ratioLst||])

{-# INLINEABLE testNatAddExtend #-}
testNatAddExtend :: CompiledCode [Integer]
testNatAddExtend =
  $$(compile [||fmap addExtend natLst||])

{-# INLINEABLE testNatRatioAddExtend #-}
testNatRatioAddExtend :: CompiledCode [Rational]
testNatRatioAddExtend =
  $$(compile [||fmap addExtend natRatioLst||])

{-# INLINEABLE testIntegerAbs #-}
testIntegerAbs :: CompiledCode [Integer]
testIntegerAbs =
  $$(compile [||fmap abs integerLst||])

{-# INLINEABLE testRationalAbs #-}
testRationalAbs :: CompiledCode [Rational]
testRationalAbs =
  $$(compile [||fmap abs ratioLst||])

{-# INLINEABLE testIntegerProjectAbs #-}
testIntegerProjectAbs :: CompiledCode [Natural]
testIntegerProjectAbs =
  $$(compile [||fmap projectAbs integerLst||])

{-# INLINEABLE testRationalProjectAbs #-}
testRationalProjectAbs :: CompiledCode [NatRatio]
testRationalProjectAbs =
  $$(compile [||fmap projectAbs ratioLst||])

{-# INLINEABLE testIntegerRestrictMay #-}
testIntegerRestrictMay :: CompiledCode [Maybe Natural]
testIntegerRestrictMay =
  $$(compile [||fmap restrictMay integerLst||])

{-# INLINEABLE testRationalRestrictMay #-}
testRationalRestrictMay :: CompiledCode [Maybe NatRatio]
testRationalRestrictMay =
  $$(compile [||fmap restrictMay ratioLst||])

{-# INLINEABLE testIntegerSignum #-}
testIntegerSignum :: CompiledCode [Integer]
testIntegerSignum =
  $$(compile [||fmap signum integerLst||])

{-# INLINEABLE testRationalSignum #-}
testRationalSignum :: CompiledCode [Rational]
testRationalSignum =
  $$(compile [||fmap signum ratioLst||])

{-# INLINEABLE testNaturalParity #-}
testNaturalParity :: CompiledCode [Parity]
testNaturalParity =
  $$(compile [||fmap parity natLst||])

{-# INLINEABLE testNatRatioFromNatural #-}
testNatRatioFromNatural :: CompiledCode [NatRatio]
testNatRatioFromNatural =
  $$(compile [||fmap NatRatio.fromNatural natLst||])

{-# INLINEABLE testNatRatioNatRatio #-}
testNatRatioNatRatio :: CompiledCode [Maybe NatRatio]
testNatRatioNatRatio =
  $$(compile [||fmap (NatRatio.natRatio natNumber) natLst||])

{-# INLINEABLE testNatRatioNumerator #-}
testNatRatioNumerator :: CompiledCode [Natural]
testNatRatioNumerator =
  $$(compile [||fmap NatRatio.numerator natRatioLst||])

{-# INLINEABLE testNatRatioDenominator #-}
testNatRatioDenominator :: CompiledCode [Natural]
testNatRatioDenominator =
  $$(compile [||fmap NatRatio.denominator natRatioLst||])

{-# INLINEABLE testNatRatioTruncate #-}
testNatRatioTruncate :: CompiledCode [Natural]
testNatRatioTruncate =
  $$(compile [||fmap NatRatio.truncate natRatioLst||])

{-# INLINEABLE testNatRatioCeiling #-}
testNatRatioCeiling :: CompiledCode [Natural]
testNatRatioCeiling =
  $$(compile [||fmap NatRatio.ceiling natRatioLst||])

{-# INLINEABLE testNatRatioRound #-}
testNatRatioRound :: CompiledCode [Natural]
testNatRatioRound =
  $$(compile [||fmap NatRatio.round natRatioLst||])

{-# INLINEABLE testNatRatioProperFraction #-}
testNatRatioProperFraction :: CompiledCode [(Natural, NatRatio)]
testNatRatioProperFraction =
  $$(compile [||fmap NatRatio.properFraction natRatioLst||])

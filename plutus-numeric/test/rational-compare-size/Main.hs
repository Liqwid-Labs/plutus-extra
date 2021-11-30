{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.Ratio as PlutusRatio
import PlutusTx.Rational qualified as Our
import PlutusTx.TH (compile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Size (fitsOnChain)

main :: IO ()
main =
  defaultMain . testGroup "Size comparisons" $
    [ testGroup
        "Eq"
        [ fitsOnChain "Plutus Rational ==" . fromCompiledCode $ pRatEq
        , fitsOnChain "Our Rational ==" . fromCompiledCode $ ourRatEq
        , fitsOnChain "Plutus Rational /=" . fromCompiledCode $ pRatNeq
        , fitsOnChain "Our Rational /=" . fromCompiledCode $ ourRatNeq
        ]
    , testGroup
        "Ord"
        [ fitsOnChain "Plutus Rational compare" . fromCompiledCode $ pRatCompare
        , fitsOnChain "Our Rational compare" . fromCompiledCode $ ourRatCompare
        , fitsOnChain "Plutus Rational <=" . fromCompiledCode $ pRatLeq
        , fitsOnChain "Our Rational <=" . fromCompiledCode $ ourRatLeq
        , fitsOnChain "Plutus Rational >=" . fromCompiledCode $ pRatGeq
        , fitsOnChain "Our Rational >=" . fromCompiledCode $ ourRatGeq
        , fitsOnChain "Plutus Rational <" . fromCompiledCode $ pRatLt
        , fitsOnChain "Our Rational <" . fromCompiledCode $ ourRatLt
        , fitsOnChain "Plutus Rational >" . fromCompiledCode $ pRatGt
        , fitsOnChain "Our Rational >" . fromCompiledCode $ ourRatGt
        , fitsOnChain "Plutus Rational min" . fromCompiledCode $ pRatMin
        , fitsOnChain "Our Rational min" . fromCompiledCode $ ourRatMin
        , fitsOnChain "Plutus Rational max" . fromCompiledCode $ pRatMax
        , fitsOnChain "Our Rational max" . fromCompiledCode $ ourRatMax
        ]
    , testGroup
        "AdditiveGroup"
        [ fitsOnChain "Plutus Rational +" . fromCompiledCode $ pRatPlus
        , fitsOnChain "Our Rational +" . fromCompiledCode $ ourRatPlus
        , fitsOnChain "Plutus Rational zero" . fromCompiledCode $ pRatZero
        , fitsOnChain "Our Rational zero" . fromCompiledCode $ ourRatZero
        , fitsOnChain "Plutus Rational -" . fromCompiledCode $ pRatMinus
        , fitsOnChain "Our Rational -" . fromCompiledCode $ ourRatMinus
        , fitsOnChain "Plutus Rational negate" . fromCompiledCode $ pRatNegate
        , fitsOnChain "Our Rational negate" . fromCompiledCode $ ourRatNegate
        , fitsOnChain "Our Rational negate (overloaded)" . fromCompiledCode $ ourRatNegateOverload
        ]
    , testGroup
        "MultiplicativeMonoid"
        [ fitsOnChain "Plutus Rational *" . fromCompiledCode $ pRatTimes
        , fitsOnChain "Our Rational *" . fromCompiledCode $ ourRatTimes
        , fitsOnChain "Plutus Rational one" . fromCompiledCode $ pRatOne
        , fitsOnChain "Our Rational one" . fromCompiledCode $ ourRatOne
        ]
    , testGroup
        "Other"
        [ fitsOnChain "Plutus Rational fromInteger" . fromCompiledCode $ pRatFromInteger
        , fitsOnChain "Our Rational fromInteger" . fromCompiledCode $ ourRatFromInteger
        , fitsOnChain "Plutus Rational numerator" . fromCompiledCode $ pRatNumerator
        , fitsOnChain "Our Rational numerator" . fromCompiledCode $ ourRatNumerator
        , fitsOnChain "Plutus Rational denominator" . fromCompiledCode $ pRatDenominator
        , fitsOnChain "Our Rational denominator" . fromCompiledCode $ ourRatDenominator
        , fitsOnChain "Plutus Rational properFraction" . fromCompiledCode $ pRatProperFrac
        , fitsOnChain "Our Rational properFraction" . fromCompiledCode $ ourRatProperFrac
        , fitsOnChain "Plutus Rational recip" . fromCompiledCode $ pRatRecip
        , fitsOnChain "Our Rational recip" . fromCompiledCode $ ourRatRecip
        , fitsOnChain "Plutus Rational abs" . fromCompiledCode $ pRatAbs
        , fitsOnChain "Our Rational abs" . fromCompiledCode $ ourRatAbs
        , fitsOnChain "Our Rational abs (overloaded)" . fromCompiledCode $ ourRatAbsOverload
        ]
    ]

-- Helpers

pRatEq :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
pRatEq = $$(compile [||(Plutus.==)||])

ourRatEq :: CompiledCode (Our.Rational -> Our.Rational -> Plutus.Bool)
ourRatEq = $$(compile [||(Plutus.==)||])

pRatNeq :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
pRatNeq = $$(compile [||(Plutus./=)||])

ourRatNeq :: CompiledCode (Our.Rational -> Our.Rational -> Plutus.Bool)
ourRatNeq = $$(compile [||(Plutus./=)||])

pRatCompare :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Ordering)
pRatCompare = $$(compile [||Plutus.compare||])

ourRatCompare :: CompiledCode (Our.Rational -> Our.Rational -> Plutus.Ordering)
ourRatCompare = $$(compile [||Plutus.compare||])

pRatLeq :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
pRatLeq = $$(compile [||(Plutus.<=)||])

ourRatLeq :: CompiledCode (Our.Rational -> Our.Rational -> Plutus.Bool)
ourRatLeq = $$(compile [||(Plutus.<=)||])

pRatGeq :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
pRatGeq = $$(compile [||(Plutus.>=)||])

ourRatGeq :: CompiledCode (Our.Rational -> Our.Rational -> Plutus.Bool)
ourRatGeq = $$(compile [||(Plutus.>=)||])

pRatLt :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
pRatLt = $$(compile [||(Plutus.<)||])

ourRatLt :: CompiledCode (Our.Rational -> Our.Rational -> Plutus.Bool)
ourRatLt = $$(compile [||(Plutus.<)||])

pRatGt :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Bool)
pRatGt = $$(compile [||(Plutus.>)||])

ourRatGt :: CompiledCode (Our.Rational -> Our.Rational -> Plutus.Bool)
ourRatGt = $$(compile [||(Plutus.>)||])

pRatMin :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
pRatMin = $$(compile [||Plutus.min||])

ourRatMin :: CompiledCode (Our.Rational -> Our.Rational -> Our.Rational)
ourRatMin = $$(compile [||Plutus.min||])

pRatMax :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
pRatMax = $$(compile [||Plutus.max||])

ourRatMax :: CompiledCode (Our.Rational -> Our.Rational -> Our.Rational)
ourRatMax = $$(compile [||Plutus.max||])

pRatPlus :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
pRatPlus = $$(compile [||(Plutus.+)||])

ourRatPlus :: CompiledCode (Our.Rational -> Our.Rational -> Our.Rational)
ourRatPlus = $$(compile [||(Plutus.+)||])

pRatZero :: CompiledCode Plutus.Rational
pRatZero = $$(compile [||Plutus.zero||])

ourRatZero :: CompiledCode Our.Rational
ourRatZero = $$(compile [||Plutus.zero||])

pRatMinus :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
pRatMinus = $$(compile [||(Plutus.-)||])

ourRatMinus :: CompiledCode (Our.Rational -> Our.Rational -> Our.Rational)
ourRatMinus = $$(compile [||(Plutus.-)||])

pRatNegate :: CompiledCode (Plutus.Rational -> Plutus.Rational)
pRatNegate = $$(compile [||Plutus.negate||])

ourRatNegate :: CompiledCode (Our.Rational -> Our.Rational)
ourRatNegate = $$(compile [||Plutus.negate||])

ourRatNegateOverload :: CompiledCode (Our.Rational -> Our.Rational)
ourRatNegateOverload = $$(compile [||Our.negate||])

pRatTimes :: CompiledCode (Plutus.Rational -> Plutus.Rational -> Plutus.Rational)
pRatTimes = $$(compile [||(Plutus.*)||])

ourRatTimes :: CompiledCode (Our.Rational -> Our.Rational -> Our.Rational)
ourRatTimes = $$(compile [||(Plutus.*)||])

pRatOne :: CompiledCode Plutus.Rational
pRatOne = $$(compile [||Plutus.one||])

ourRatOne :: CompiledCode Our.Rational
ourRatOne = $$(compile [||Plutus.one||])

pRatFromInteger :: CompiledCode (Integer -> Plutus.Rational)
pRatFromInteger = $$(compile [||Plutus.fromInteger||])

ourRatFromInteger :: CompiledCode (Integer -> Our.Rational)
ourRatFromInteger = $$(compile [||Our.fromInteger||])

pRatNumerator :: CompiledCode (Plutus.Rational -> Integer)
pRatNumerator = $$(compile [||PlutusRatio.numerator||])

ourRatNumerator :: CompiledCode (Our.Rational -> Integer)
ourRatNumerator = $$(compile [||Our.numerator||])

pRatDenominator :: CompiledCode (Plutus.Rational -> Integer)
pRatDenominator = $$(compile [||PlutusRatio.denominator||])

ourRatDenominator :: CompiledCode (Our.Rational -> Integer)
ourRatDenominator = $$(compile [||Our.denominator||])

pRatProperFrac :: CompiledCode (Plutus.Rational -> (Integer, Plutus.Rational))
pRatProperFrac = $$(compile [||PlutusRatio.properFraction||])

ourRatProperFrac :: CompiledCode (Our.Rational -> (Integer, Our.Rational))
ourRatProperFrac = $$(compile [||Our.properFraction||])

pRatRecip :: CompiledCode (Plutus.Rational -> Plutus.Rational)
pRatRecip = $$(compile [||PlutusRatio.recip||])

ourRatRecip :: CompiledCode (Our.Rational -> Our.Rational)
ourRatRecip = $$(compile [||Our.recip||])

pRatAbs :: CompiledCode (Plutus.Rational -> Plutus.Rational)
pRatAbs = $$(compile [||PlutusRatio.abs||])

ourRatAbs :: CompiledCode (Our.Rational -> Our.Rational)
ourRatAbs = $$(compile [||PlutusRatio.abs||])

ourRatAbsOverload :: CompiledCode (Our.Rational -> Our.Rational)
ourRatAbsOverload = $$(compile [||Our.abs||])

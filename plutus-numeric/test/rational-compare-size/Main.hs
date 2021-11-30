{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.Ratio as PlutusRatio
import PlutusTx.Rational qualified as Our
import PlutusTx.TH (compile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Size (fitsUnder)

main :: IO ()
main =
  defaultMain . testGroup "Rational (us versus Plutus)" $
    [ testGroup
        "Eq"
        [ fitsUnder "==" (fromCompiledCode ourRatEq) (fromCompiledCode pRatEq)
        , fitsUnder "/=" (fromCompiledCode ourRatNeq) (fromCompiledCode pRatNeq)
        ]
    , testGroup
        "Ord"
        [ fitsUnder "compare" (fromCompiledCode ourRatCompare) (fromCompiledCode pRatCompare)
        , fitsUnder "<=" (fromCompiledCode ourRatLeq) (fromCompiledCode pRatLeq)
        , fitsUnder ">=" (fromCompiledCode ourRatGeq) (fromCompiledCode pRatGeq)
        , fitsUnder "<" (fromCompiledCode ourRatLt) (fromCompiledCode pRatLt)
        , fitsUnder ">" (fromCompiledCode ourRatGt) (fromCompiledCode pRatGt)
        , fitsUnder "min" (fromCompiledCode ourRatMin) (fromCompiledCode pRatMin)
        , fitsUnder "max" (fromCompiledCode ourRatMax) (fromCompiledCode pRatMax)
        ]
    , testGroup
        "AdditiveGroup"
        [ fitsUnder "+" (fromCompiledCode ourRatPlus) (fromCompiledCode pRatPlus)
        , fitsUnder "zero" (fromCompiledCode ourRatZero) (fromCompiledCode pRatZero)
        , fitsUnder "-" (fromCompiledCode ourRatMinus) (fromCompiledCode pRatMinus)
        , fitsUnder "negate" (fromCompiledCode ourRatNegate) (fromCompiledCode pRatNegate)
        , fitsUnder
            "negate (overloaded)"
            (fromCompiledCode ourRatNegateOverload)
            (fromCompiledCode pRatNegate)
        ]
    , testGroup
        "MultiplicativeMonoid"
        [ fitsUnder "*" (fromCompiledCode ourRatTimes) (fromCompiledCode pRatTimes)
        , fitsUnder "one" (fromCompiledCode ourRatOne) (fromCompiledCode pRatOne)
        ]
    , testGroup
        "Other"
        [ fitsUnder "%" (fromCompiledCode ourRatMk) (fromCompiledCode pRatMk)
        , fitsUnder
            "fromInteger"
            (fromCompiledCode ourRatFromInteger)
            (fromCompiledCode pRatFromInteger)
        , fitsUnder
            "numerator"
            (fromCompiledCode ourRatNumerator)
            (fromCompiledCode pRatNumerator)
        , fitsUnder
            "denominator"
            (fromCompiledCode ourRatDenominator)
            (fromCompiledCode pRatDenominator)
        , fitsUnder
            "round"
            (fromCompiledCode ourRatRound)
            (fromCompiledCode pRatRound)
        , fitsUnder
            "truncate"
            (fromCompiledCode ourRatTruncate)
            (fromCompiledCode pRatTruncate)
        , fitsUnder
            "properFraction"
            (fromCompiledCode ourRatProperFrac)
            (fromCompiledCode pRatProperFrac)
        , fitsUnder
            "recip"
            (fromCompiledCode ourRatRecip)
            (fromCompiledCode pRatRecip)
        , fitsUnder
            "abs"
            (fromCompiledCode ourRatAbs)
            (fromCompiledCode pRatAbs)
        , fitsUnder
            "abs (overloaded)"
            (fromCompiledCode ourRatAbsOverload)
            (fromCompiledCode pRatAbs)
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

pRatMk :: CompiledCode (Integer -> Integer -> Plutus.Rational)
pRatMk = $$(compile [||(PlutusRatio.%)||])

ourRatMk :: CompiledCode (Integer -> Integer -> Our.Rational)
ourRatMk = $$(compile [||(Our.%)||])

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

pRatRound :: CompiledCode (Plutus.Rational -> Integer)
pRatRound = $$(compile [||PlutusRatio.round||])

ourRatRound :: CompiledCode (Our.Rational -> Integer)
ourRatRound = $$(compile [||Our.round||])

ourRatDenominator :: CompiledCode (Our.Rational -> Integer)
ourRatDenominator = $$(compile [||Our.denominator||])

pRatTruncate :: CompiledCode (Plutus.Rational -> Integer)
pRatTruncate = $$(compile [||PlutusRatio.truncate||])

ourRatTruncate :: CompiledCode (Our.Rational -> Integer)
ourRatTruncate = $$(compile [||Our.truncate||])

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

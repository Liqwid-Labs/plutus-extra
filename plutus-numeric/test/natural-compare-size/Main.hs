{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Test.Tasty.ExpectedFailure (expectFailBecause)
import Plutus.V1.Ledger.Scripts (fromCompiledCode)
import PlutusTx.Code (CompiledCode)
import PlutusTx.IsData.Class (
  fromBuiltinData,
  toBuiltinData,
  unsafeFromBuiltinData,
 )
import PlutusTx.Natural (Natural)
import PlutusTx.Numeric.Extra (
  powNat,
  (^-),
  divMod,
 )
import PlutusTx.Prelude qualified as Plutus
import PlutusTx.TH (compile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Plutus.Size (fitsUnder)
import Prelude hiding (Rational, (/), divMod)

main :: IO ()
main =
  defaultMain . testGroup "Natural vs Integer" $
    [ testGroup
        "Eq"
        [ fitsUnder "==" (fromCompiledCode natEq) (fromCompiledCode iEq)
        , fitsUnder "/=" (fromCompiledCode natNeq) (fromCompiledCode iNeq)
        ]
    , testGroup
        "Ord"
        [ fitsUnder "compare" (fromCompiledCode natCompare) (fromCompiledCode iCompare)
        , fitsUnder "<=" (fromCompiledCode natLE) (fromCompiledCode iLE)
        , fitsUnder ">=" (fromCompiledCode natGE) (fromCompiledCode iGE)
        , fitsUnder "<" (fromCompiledCode natLT) (fromCompiledCode iLT)
        , fitsUnder ">" (fromCompiledCode natGT) (fromCompiledCode iGT)
        , fitsUnder "min" (fromCompiledCode natMin) (fromCompiledCode iMin)
        , fitsUnder "max" (fromCompiledCode natMax) (fromCompiledCode iMax)
        ]
    , testGroup
        "Additive"
        [ fitsUnder "+" (fromCompiledCode natPlus) (fromCompiledCode iPlus)
        , fitsUnder "zero" (fromCompiledCode natZero) (fromCompiledCode iZero)
        , expectFailBecause "monus requires more checks" . 
            fitsUnder "^- vs -" (fromCompiledCode natMonus) . 
            fromCompiledCode $ iMinus
        ]
    , testGroup
        "Multiplicative"
        [ fitsUnder "*" (fromCompiledCode natTimes) (fromCompiledCode iTimes)
        , fitsUnder "one" (fromCompiledCode natOne) (fromCompiledCode iOne)
        , fitsUnder "powNat" (fromCompiledCode natPowNat) (fromCompiledCode iPowNat)
        ]
    , testGroup
        "IntegralDomain"
        [ fitsUnder "divMod" (fromCompiledCode natDivMod) (fromCompiledCode iDivMod)
        ]
    , testGroup
        "Serialization"
        [ fitsUnder
            "toBuiltinData"
            (fromCompiledCode natToBuiltinData)
            (fromCompiledCode iToBuiltinData)
        , expectFailBecause "Natural requires more checks" . 
            fitsUnder "fromBuiltinData" (fromCompiledCode natFromBuiltinData) .
            fromCompiledCode $ iFromBuiltinData
        , expectFailBecause "Natural requires more checks" . 
            fitsUnder "unsafeFromBuiltinData" (fromCompiledCode natUnsafeFromBuiltinData) .
            fromCompiledCode $ iUnsafeFromBuiltinData
        ]
    ]

-- Helpers

natEq :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natEq = $$(compile [||(Plutus.==)||])

natNeq :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natNeq = $$(compile [||(Plutus.==)||])

natCompare :: CompiledCode (Natural -> Natural -> Plutus.Ordering)
natCompare = $$(compile [||Plutus.compare||])

natLE :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natLE = $$(compile [||(Plutus.<=)||])

natGE :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natGE = $$(compile [||(Plutus.>=)||])

natLT :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natLT = $$(compile [||(Plutus.<)||])

natGT :: CompiledCode (Natural -> Natural -> Plutus.Bool)
natGT = $$(compile [||(Plutus.>)||])

natMin :: CompiledCode (Natural -> Natural -> Natural)
natMin = $$(compile [||Plutus.min||])

natMax :: CompiledCode (Natural -> Natural -> Natural)
natMax = $$(compile [||Plutus.max||])

natPlus :: CompiledCode (Natural -> Natural -> Natural)
natPlus = $$(compile [||(Plutus.+)||])

natZero :: CompiledCode Natural
natZero = $$(compile [||Plutus.zero||])

natMonus :: CompiledCode (Natural -> Natural -> Natural)
natMonus = $$(compile [||(^-)||])

natTimes :: CompiledCode (Natural -> Natural -> Natural)
natTimes = $$(compile [||(Plutus.*)||])

natOne :: CompiledCode Natural
natOne = $$(compile [||Plutus.one||])

natToBuiltinData :: CompiledCode (Natural -> Plutus.BuiltinData)
natToBuiltinData = $$(compile [||toBuiltinData||])

natFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Maybe Natural)
natFromBuiltinData = $$(compile [||fromBuiltinData||])

natUnsafeFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Natural)
natUnsafeFromBuiltinData = $$(compile [||unsafeFromBuiltinData||])

natPowNat :: CompiledCode (Natural -> Natural -> Natural)
natPowNat = $$(compile [||powNat||])

natDivMod :: CompiledCode (Natural -> Natural -> (Natural, Natural))
natDivMod = $$(compile [||divMod||])

iEq :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iEq = $$(compile [||(Plutus.==)||])

iNeq :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iNeq = $$(compile [||(Plutus.==)||])

iCompare :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Ordering)
iCompare = $$(compile [||Plutus.compare||])

iLE :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iLE = $$(compile [||(Plutus.<=)||])

iGE :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iGE = $$(compile [||(Plutus.>=)||])

iLT :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iLT = $$(compile [||(Plutus.<)||])

iGT :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Bool)
iGT = $$(compile [||(Plutus.>)||])

iMin :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iMin = $$(compile [||Plutus.min||])

iMax :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iMax = $$(compile [||Plutus.max||])

iPlus :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iPlus = $$(compile [||(Plutus.+)||])

iZero :: CompiledCode Plutus.Integer
iZero = $$(compile [||Plutus.zero||])

iMinus :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iMinus = $$(compile [||(Plutus.-)||])

iTimes :: CompiledCode (Plutus.Integer -> Plutus.Integer -> Plutus.Integer)
iTimes = $$(compile [||(Plutus.*)||])

iOne :: CompiledCode Plutus.Integer
iOne = $$(compile [||Plutus.one||])

iToBuiltinData :: CompiledCode (Plutus.Integer -> Plutus.BuiltinData)
iToBuiltinData = $$(compile [||toBuiltinData||])

iFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Maybe Plutus.Integer)
iFromBuiltinData = $$(compile [||fromBuiltinData||])

iUnsafeFromBuiltinData :: CompiledCode (Plutus.BuiltinData -> Plutus.Integer)
iUnsafeFromBuiltinData = $$(compile [||unsafeFromBuiltinData||])

iPowNat :: CompiledCode (Plutus.Integer -> Natural -> Plutus.Integer)
iPowNat = $$(compile [||powNat||])

iDivMod :: CompiledCode (Plutus.Integer -> Plutus.Integer -> (Plutus.Integer, Plutus.Integer))
iDivMod = $$(compile [||divMod||])

-- Full of portrayals
module Data.Portray.Plutus (
  portrayBuiltinData,
) where

import Data.Portray (
  FactorPortrayal (FactorPortrayal),
  Portrayal (Apply, Atom, List, Record),
 )
import Data.Text (pack)
import PlutusTx.Builtins (
  BuiltinByteString,
  BuiltinData,
  matchData,
 )

portrayBuiltinData :: BuiltinData -> Portrayal
portrayBuiltinData dat =
  matchData
    dat
    portrayConstr
    portrayMap
    portrayList
    portrayInteger
    portrayBytes
  where
    portrayConstr :: Integer -> [BuiltinData] -> Portrayal
    portrayConstr ix =
      Apply (Atom . pack $ "Constr " <> show ix)
        . fmap portrayBuiltinData
    portrayMap :: [(BuiltinData, BuiltinData)] -> Portrayal
    portrayMap fields = Apply (Atom "Map") [List . fmap portrayField $ fields]
    portrayList :: [BuiltinData] -> Portrayal
    portrayList = List . fmap portrayBuiltinData
    portrayInteger :: Integer -> Portrayal
    portrayInteger = Atom . pack . show
    portrayBytes :: BuiltinByteString -> Portrayal
    portrayBytes = Atom . pack . show
    portrayField :: (BuiltinData, BuiltinData) -> Portrayal
    portrayField (key, val) =
      Record
        "Entry"
        [ FactorPortrayal "Key" . portrayBuiltinData $ key
        , FactorPortrayal "Value" . portrayBuiltinData $ val
        ]

-- Full of portrayals
module Data.Portray.Plutus (
  portrayBuiltinData,
  portrayBuiltinByteString,
  portrayScriptContext,
  portrayTxInfo,
  portrayTxInInfo,
  portrayTxOut,
  portrayValue,
  portrayDCert,
  portrayPOSIXTimeRange,
  portrayPubKeyHash,
  portrayTxId,
  portrayStakingCredential,
  portrayDatumHash,
  portrayDatum,
  portrayTxOutRef,
  portrayAddress,
  portrayAssocMap,
  portrayCurrencySymbol,
  portrayTokenName,
  portrayCredential,
  portrayValidatorHash,
  portrayPOSIXTime,
  portrayScriptPurpose,
) where

import Data.Kind (Type)
import Data.Portray (
  FactorPortrayal (FactorPortrayal),
  Portrayal (Apply, Atom, List, Record, Tuple),
  portray,
 )
import Data.Text (pack)
import Plutus.V1.Ledger.Address (addressCredential, addressStakingCredential)
import Plutus.V1.Ledger.Contexts (
  ScriptContext (
    scriptContextPurpose,
    scriptContextTxInfo
  ),
  ScriptPurpose (Certifying, Minting, Rewarding, Spending),
  TxInInfo (
    txInInfoOutRef,
    txInInfoResolved
  ),
  TxInfo (
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
 )
import Plutus.V1.Ledger.Credential (
  Credential (PubKeyCredential, ScriptCredential),
  StakingCredential (StakingHash, StakingPtr),
 )
import Plutus.V1.Ledger.Crypto (PubKeyHash (getPubKeyHash))
import Plutus.V1.Ledger.DCert (
  DCert (
    DCertDelegDeRegKey,
    DCertDelegDelegate,
    DCertDelegRegKey,
    DCertGenesis,
    DCertMir,
    DCertPoolRegister,
    DCertPoolRetire
  ),
 )
import Plutus.V1.Ledger.Interval (
  Extended (Finite, NegInf, PosInf),
  Interval (ivFrom, ivTo),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import Plutus.V1.Ledger.Scripts (
  Datum (getDatum),
  DatumHash (DatumHash),
  ValidatorHash (ValidatorHash),
 )
import Plutus.V1.Ledger.Time (POSIXTime (getPOSIXTime))
import Plutus.V1.Ledger.Tx (
  Address,
  TxOut (txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef (txOutRefId, txOutRefIdx),
 )
import Plutus.V1.Ledger.TxId (TxId (getTxId))
import Plutus.V1.Ledger.Value (
  CurrencySymbol (unCurrencySymbol),
  TokenName (unTokenName),
  Value (getValue),
 )
import PlutusTx.AssocMap (Map)
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins (
  BuiltinByteString,
  BuiltinData,
  matchData,
 )

portrayScriptContext :: ScriptContext -> Portrayal
portrayScriptContext sc =
  Record "ScriptContext" . fmap (uncurry FactorPortrayal) $
    [ ("scriptContextTxInfo", portrayTxInfo . scriptContextTxInfo $ sc)
    , ("scriptContextPurpose", portrayScriptPurpose . scriptContextPurpose $ sc)
    ]

portrayTxInfo :: TxInfo -> Portrayal
portrayTxInfo info =
  Record "TxInfo" . fmap (uncurry FactorPortrayal) $
    [ ("txInfoInputs", List . fmap portrayTxInInfo . txInfoInputs $ info)
    , ("txInfoOutputs", List . fmap portrayTxOut . txInfoOutputs $ info)
    , ("txInfoFee", portrayValue . txInfoFee $ info)
    , ("txInfoMint", portrayValue . txInfoMint $ info)
    , ("txInfoDCert", List . fmap portrayDCert . txInfoDCert $ info)
    , ("txInfoWdrl", List . fmap (Tuple . goWdrl) . txInfoWdrl $ info)
    , ("txInfoValidRange", portrayPOSIXTimeRange . txInfoValidRange $ info)
    , ("txInfoSignatories", List . fmap portrayPubKeyHash . txInfoSignatories $ info)
    , ("txInfoData", List . fmap (Tuple . goInfoData) . txInfoData $ info)
    , ("txInfoId", portrayTxId . txInfoId $ info)
    ]
  where
    goWdrl :: (StakingCredential, Integer) -> [Portrayal]
    goWdrl (sc, i) = [portrayStakingCredential sc, portray i]
    goInfoData :: (DatumHash, Datum) -> [Portrayal]
    goInfoData (dh, d) = [portrayDatumHash dh, portrayDatum d]

portrayTxInInfo :: TxInInfo -> Portrayal
portrayTxInInfo info =
  Record "TxInInfo" . fmap (uncurry FactorPortrayal) $
    [ ("txInInfoOutRef", portrayTxOutRef . txInInfoOutRef $ info)
    , ("txInInfoResolved", portrayTxOut . txInInfoResolved $ info)
    ]

portrayTxOut :: TxOut -> Portrayal
portrayTxOut out =
  Record "TxOut" . fmap (uncurry FactorPortrayal) $
    [ ("txOutAddress", portrayAddress . txOutAddress $ out)
    , ("txOutValue", portrayValue . txOutValue $ out)
    , ("txOutDatumHash", portrayMaybe portrayDatumHash . txOutDatumHash $ out)
    ]

portrayValue :: Value -> Portrayal
portrayValue val =
  Record
    "Value"
    [ FactorPortrayal "getValue"
        . portrayAssocMap portrayCurrencySymbol (portrayAssocMap portrayTokenName portray)
        . getValue
        $ val
    ]

portrayDCert :: DCert -> Portrayal
portrayDCert = \case
  DCertDelegRegKey sc ->
    Apply "DCertDelegRegKey" [portrayStakingCredential sc]
  DCertDelegDeRegKey sc ->
    Apply "DCertDelegDeRegKey" [portrayStakingCredential sc]
  DCertDelegDelegate sc pkh ->
    Apply
      "DCertDelegDelegate"
      [ portrayStakingCredential sc
      , portrayPubKeyHash pkh
      ]
  DCertPoolRegister poolId poolVFR ->
    Apply
      "DCertPoolRegister"
      [ portrayPubKeyHash poolId
      , portrayPubKeyHash poolVFR
      ]
  DCertPoolRetire pkh i ->
    Apply
      "DCertPoolRetire"
      [ portrayPubKeyHash pkh
      , portray i
      ]
  DCertGenesis -> Apply "DCertGenesis" []
  DCertMir -> Apply "DCertMir" []

portrayPOSIXTimeRange :: Interval POSIXTime -> Portrayal
portrayPOSIXTimeRange interval =
  Record "Interval" . fmap (uncurry FactorPortrayal) $
    [ ("ivFrom", portrayLower . ivFrom $ interval)
    , ("ivTo", portrayUpper . ivTo $ interval)
    ]

portrayPubKeyHash :: PubKeyHash -> Portrayal
portrayPubKeyHash pkh =
  Apply "PubKeyHash" [portrayBuiltinByteString . getPubKeyHash $ pkh]

portrayTxId :: TxId -> Portrayal
portrayTxId txId =
  Apply "TxId" [portrayBuiltinByteString . getTxId $ txId]

portrayStakingCredential :: StakingCredential -> Portrayal
portrayStakingCredential = \case
  StakingHash cred -> Apply "StakingHash" [portrayCredential cred]
  StakingPtr i j k -> Apply "StakingPtr" [portray i, portray j, portray k]

portrayDatumHash :: DatumHash -> Portrayal
portrayDatumHash (DatumHash bs) =
  Apply "DatumHash" [portrayBuiltinByteString bs]

portrayDatum :: Datum -> Portrayal
portrayDatum dat =
  Apply "Datum" [portrayBuiltinData . getDatum $ dat]

portrayTxOutRef :: TxOutRef -> Portrayal
portrayTxOutRef txOutRef =
  Record "TxOutRef" . fmap (uncurry FactorPortrayal) $
    [ ("txOutRefId", portrayTxId . txOutRefId $ txOutRef)
    , ("txOutRefIdx", portray . txOutRefIdx $ txOutRef)
    ]

portrayAddress :: Address -> Portrayal
portrayAddress addr =
  Record "Address" . fmap (uncurry FactorPortrayal) $
    [ ("addressCredential", portrayCredential . addressCredential $ addr)
    , ("addressStakingCredential", portrayMaybe portrayStakingCredential . addressStakingCredential $ addr)
    ]

portrayCurrencySymbol :: CurrencySymbol -> Portrayal
portrayCurrencySymbol sym =
  Apply "CurrencySymbol" [portrayBuiltinByteString . unCurrencySymbol $ sym]

portrayTokenName :: TokenName -> Portrayal
portrayTokenName tokName =
  Apply "TokenName" [portrayBuiltinByteString . unTokenName $ tokName]

portrayAssocMap ::
  forall (a :: Type) (b :: Type).
  (a -> Portrayal) ->
  (b -> Portrayal) ->
  Map a b ->
  Portrayal
portrayAssocMap fKey fVal = List . fmap (Tuple . go) . AMap.toList
  where
    go :: (a, b) -> [Portrayal]
    go (k, v) = [fKey k, fVal v]

portrayCredential :: Credential -> Portrayal
portrayCredential = \case
  PubKeyCredential pkh ->
    Apply "PubKeyCredential" [portrayPubKeyHash pkh]
  ScriptCredential vh ->
    Apply "ScriptCredential" [portrayValidatorHash vh]

portrayValidatorHash :: ValidatorHash -> Portrayal
portrayValidatorHash (ValidatorHash bs) =
  Apply "ValidatorHash" [portrayBuiltinByteString bs]

portrayScriptPurpose :: ScriptPurpose -> Portrayal
portrayScriptPurpose = \case
  Minting sym -> Apply "Minting" [portrayCurrencySymbol sym]
  Spending ref -> Apply "Spending" [portrayTxOutRef ref]
  Rewarding cred -> Apply "Rewarding" [portrayStakingCredential cred]
  Certifying cert -> Apply "Certifying" [portrayDCert cert]

portrayBuiltinData :: BuiltinData -> Portrayal
portrayBuiltinData dat =
  matchData
    dat
    portrayConstr
    portrayMap
    portrayList
    portrayInteger
    portrayBuiltinByteString
  where
    portrayConstr :: Integer -> [BuiltinData] -> Portrayal
    portrayConstr ix =
      Apply (Atom . pack $ "Constr " <> show ix)
        . fmap portrayBuiltinData
    portrayMap :: [(BuiltinData, BuiltinData)] -> Portrayal
    portrayMap fields = Apply "Map" [List . fmap portrayField $ fields]
    portrayList :: [BuiltinData] -> Portrayal
    portrayList = List . fmap portrayBuiltinData
    portrayInteger :: Integer -> Portrayal
    portrayInteger = Atom . pack . show
    portrayField :: (BuiltinData, BuiltinData) -> Portrayal
    portrayField (key, val) =
      Record
        "Entry"
        [ FactorPortrayal "Key" . portrayBuiltinData $ key
        , FactorPortrayal "Value" . portrayBuiltinData $ val
        ]

portrayBuiltinByteString :: BuiltinByteString -> Portrayal
portrayBuiltinByteString = Atom . pack . show

portrayPOSIXTime :: POSIXTime -> Portrayal
portrayPOSIXTime t =
  Apply "POSIXTime" [portray . getPOSIXTime $ t]

-- Helpers

portrayMaybe ::
  forall (a :: Type).
  (a -> Portrayal) ->
  Maybe a ->
  Portrayal
portrayMaybe f = \case
  Nothing -> Apply "Nothing" []
  Just x -> Apply "Just" [f x]

portrayLower :: LowerBound POSIXTime -> Portrayal
portrayLower (LowerBound ext clos) =
  Apply "LowerBound" [portrayExtended ext, portray clos]

portrayUpper :: UpperBound POSIXTime -> Portrayal
portrayUpper (UpperBound ext clos) =
  Apply "UpperBound" [portrayExtended ext, portray clos]

portrayExtended :: Extended POSIXTime -> Portrayal
portrayExtended = \case
  NegInf -> Apply "NegInf" []
  Finite x -> Apply "Finite" [portrayPOSIXTime x]
  PosInf -> Apply "PosInf" []

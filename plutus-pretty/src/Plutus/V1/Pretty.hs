{-# LANGUAGE Trustworthy #-}

{- |
 Module: Plutus.V1.Pretty
 Copyright: (C) MLabs 2021
 License: Apache 2.0
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 A collection of helper functions for converting Plutus types to
 'Pretty.Value'.
-}
module Plutus.V1.Pretty (
  scriptContextToValue,
  txInfoToValue,
  scriptPurposeToValue,
  txInInfoToValue,
  txOutToValue,
  valueToValue,
  dCertToValue,
  pubKeyHashToValue,
  txIdToValue,
  currencySymbolToValue,
  txOutRefToValue,
  stakingCredentialToValue,
  datumHashToValue,
  datumToValue,
  posixTimeToValue,
  addressToValue,
  tokenNameToValue,
  credentialToValue,
  validatorHashToValue,
) where

import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Contexts (
  ScriptContext (
    scriptContextPurpose,
    scriptContextTxInfo
  ),
  ScriptPurpose (
    Certifying,
    Minting,
    Rewarding,
    Spending
  ),
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
  TxOut (
    txOutAddress,
    txOutDatumHash,
    txOutValue
  ),
  TxOutRef (
    txOutRefId,
    txOutRefIdx
  ),
 )
import Plutus.V1.Ledger.Credential (
  Credential (
    PubKeyCredential,
    ScriptCredential
  ),
  StakingCredential (
    StakingHash,
    StakingPtr
  ),
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
import Plutus.V1.Ledger.Scripts (
  Datum (getDatum),
  DatumHash (DatumHash),
  ValidatorHash (ValidatorHash),
 )
import Plutus.V1.Ledger.Time (POSIXTime (getPOSIXTime))
import Plutus.V1.Ledger.TxId (TxId (getTxId))
import Plutus.V1.Ledger.Value (
  CurrencySymbol (unCurrencySymbol),
  TokenName (unTokenName),
  Value (getValue),
 )
import Text.Show.Pretty qualified as Pretty
import Text.Show.Pretty.Extra (liftPrettyVal, liftPrettyVal2)
import Prelude

{- | Transforms a 'ScriptContext' into a 'Pretty.Value'.

 @since 1.0
-}
scriptContextToValue :: ScriptContext -> Pretty.Value
scriptContextToValue sc =
  Pretty.Rec
    "ScriptContext"
    [ ("scriptContextTxInfo", txInfoToValue . scriptContextTxInfo $ sc)
    , ("scriptContextPurpose", scriptPurposeToValue . scriptContextPurpose $ sc)
    ]

{- | Transforms a 'TxInfo' into a 'Pretty.Value'.

 @since 1.0
-}
txInfoToValue :: TxInfo -> Pretty.Value
txInfoToValue txi =
  Pretty.Rec
    "TxInfo"
    [ ("txInfoInputs", liftPrettyVal txInInfoToValue . txInfoInputs $ txi)
    , ("txInfoOutputs", liftPrettyVal txOutToValue . txInfoOutputs $ txi)
    , ("txInfoFee", valueToValue . txInfoFee $ txi)
    , ("txInfoMint", valueToValue . txInfoMint $ txi)
    , ("txInfoDCert", liftPrettyVal dCertToValue . txInfoDCert $ txi)
    ,
      ( "txInfoWdrl"
      , liftPrettyVal (liftPrettyVal2 stakingCredentialToValue Pretty.prettyVal)
          . txInfoWdrl
          $ txi
      )
    , ("txInfoValidRange", liftPrettyVal posixTimeToValue . txInfoValidRange $ txi)
    , ("txInfoSignatories", liftPrettyVal pubKeyHashToValue . txInfoSignatories $ txi)
    ,
      ( "txInfoData"
      , liftPrettyVal (liftPrettyVal2 datumHashToValue datumToValue)
          . txInfoData
          $ txi
      )
    , ("txInfoId", txIdToValue . txInfoId $ txi)
    ]

{- | Transforms a 'ScriptPurpose' into a 'Pretty.Value'.

 @since 1.0
-}
scriptPurposeToValue :: ScriptPurpose -> Pretty.Value
scriptPurposeToValue = \case
  Minting cs -> Pretty.Con "Minting" [currencySymbolToValue cs]
  Spending txOutRef -> Pretty.Con "Spending" [txOutRefToValue txOutRef]
  Rewarding sc -> Pretty.Con "Rewarding" [stakingCredentialToValue sc]
  Certifying dc -> Pretty.Con "Certifying" [dCertToValue dc]

{- | Transforms a 'TxInInfo' into a 'Pretty.Value'.

 @since 1.0
-}
txInInfoToValue :: TxInInfo -> Pretty.Value
txInInfoToValue tii =
  Pretty.Rec
    "TxInInfo"
    [ ("txInInfoOutRef", txOutRefToValue . txInInfoOutRef $ tii)
    , ("txInInfoResolved", txOutToValue . txInInfoResolved $ tii)
    ]

{- | Transforms a 'TxOut' into a 'Pretty.Value'.

 @since 1.0
-}
txOutToValue :: TxOut -> Pretty.Value
txOutToValue txo =
  Pretty.Rec
    "TxOut"
    [ ("txOutAddress", addressToValue . txOutAddress $ txo)
    , ("txOutValue", valueToValue . txOutValue $ txo)
    , ("txOutDatumHash", liftPrettyVal datumHashToValue . txOutDatumHash $ txo)
    ]

{- | Transforms a Plutus 'Value' into a @pretty-show@ 'Pretty.Value'.

 @since 1.0
-}
valueToValue :: Value -> Pretty.Value
valueToValue val =
  Pretty.Con
    "Value"
    [ liftPrettyVal2 currencySymbolToValue (liftPrettyVal2 tokenNameToValue Pretty.prettyVal)
        . getValue
        $ val
    ]

{- | Transforms a 'DCert' into a 'Pretty.Value'.

 @since 1.0
-}
dCertToValue :: DCert -> Pretty.Value
dCertToValue = \case
  DCertDelegRegKey sc ->
    Pretty.Con "DCertDelegRegKey" [stakingCredentialToValue sc]
  DCertDelegDeRegKey sc ->
    Pretty.Con "DCertDelegDeRegKey" [stakingCredentialToValue sc]
  DCertDelegDelegate sc pkh ->
    Pretty.Con
      "DCertDelegDelegate"
      [ stakingCredentialToValue sc
      , pubKeyHashToValue pkh
      ]
  DCertPoolRegister poolId poolVFR ->
    Pretty.Con
      "DCertPoolRegister"
      [ pubKeyHashToValue poolId
      , pubKeyHashToValue poolVFR
      ]
  DCertPoolRetire pkh i ->
    Pretty.Con
      "DCertPoolRetire"
      [ pubKeyHashToValue pkh
      , Pretty.prettyVal i
      ]
  DCertGenesis -> Pretty.Con "DCertGenesis" []
  DCertMir -> Pretty.Con "DCertMir" []

{- | Transforms a 'PubKeyHash' into a 'Pretty.Value'.

 @since 1.0
-}
pubKeyHashToValue :: PubKeyHash -> Pretty.Value
pubKeyHashToValue pkh =
  Pretty.Con "PubKeyHash" [Pretty.String . show . getPubKeyHash $ pkh]

{- | Transforms a 'TxId' into a 'Pretty.Value'.

 @since 1.0
-}
txIdToValue :: TxId -> Pretty.Value
txIdToValue ti =
  Pretty.Con "TxId" [Pretty.String . show . getTxId $ ti]

{- | Transforms a 'CurrencySymbol' into a 'Pretty.Value'.

 @since 1.0
-}
currencySymbolToValue :: CurrencySymbol -> Pretty.Value
currencySymbolToValue cs =
  Pretty.Con "CurrencySymbol" [Pretty.String . show . unCurrencySymbol $ cs]

{- | Transforms a 'TxOutRef' into a 'Pretty.Value'.

 @since 1.0
-}
txOutRefToValue :: TxOutRef -> Pretty.Value
txOutRefToValue tor =
  Pretty.Rec
    "TxOutRef"
    [ ("txOutRefId", txIdToValue . txOutRefId $ tor)
    , ("txOutRefIdx", Pretty.prettyVal . txOutRefIdx $ tor)
    ]

{- | Transforms a 'StakingCredential' into a 'Pretty.Value'.

 @since 1.0
-}
stakingCredentialToValue :: StakingCredential -> Pretty.Value
stakingCredentialToValue = \case
  StakingHash cred -> Pretty.Con "StakingHash" [credentialToValue cred]
  StakingPtr i1 i2 i3 ->
    Pretty.Con "StakingPtr" . fmap Pretty.prettyVal $ [i1, i2, i3]

{- | Transforms a 'DatumHash' into a 'Pretty.Value'.

 @since 1.0
-}
datumHashToValue :: DatumHash -> Pretty.Value
datumHashToValue (DatumHash dh) =
  Pretty.Con "DatumHash" [Pretty.String . show $ dh]

{- | Transforms a 'Datum' into a 'Pretty.Value'.

 @since 1.0
-}
datumToValue :: Datum -> Pretty.Value
datumToValue d =
  Pretty.Con "Datum" [Pretty.String . show . getDatum $ d]

{- | Transforms a 'POSIXTime' into a 'Pretty.Value'.

 @since 1.0
-}
posixTimeToValue :: POSIXTime -> Pretty.Value
posixTimeToValue t =
  Pretty.Con "POSIXTime" [Pretty.prettyVal . getPOSIXTime $ t]

{- | Transforms an 'Address' into a 'Pretty.Value'.

 = Note

 Addresses are opaque; we display them as a one-argument constructor, 
 containing their 'Show'-based representation.

 @since 1.0
-}
addressToValue :: Address -> Pretty.Value
addressToValue addr = Pretty.Con "Address" [Pretty.prettyVal . show $ addr]

{- | Transforms a 'TokenName' into a 'Pretty.Value'.

 @since 1.0
-}
tokenNameToValue :: TokenName -> Pretty.Value
tokenNameToValue tn =
  Pretty.Con "TokenName" [Pretty.String . show . unTokenName $ tn]

{- | Transforms a 'Credential' into a 'Pretty.Value'.

 @since 1.0
-}
credentialToValue :: Credential -> Pretty.Value
credentialToValue = \case
  PubKeyCredential pkh ->
    Pretty.Con "PubKeyCredential" [pubKeyHashToValue pkh]
  ScriptCredential vh ->
    Pretty.Con "ScriptCredential" [validatorHashToValue vh]

{- | Transforms a 'ValidatorHash' into a 'Pretty.Value'.

 @since 1.0
-}
validatorHashToValue :: ValidatorHash -> Pretty.Value
validatorHashToValue (ValidatorHash vh) =
  Pretty.Con "ValidatorHash" [Pretty.String . show $ vh]

module PlutusTx.Skeleton.Internal (
  Skeleton (..),
  Skeletal (..),
) where

import GHC.Exts (fromString)
import Plutus.V1.Ledger.Address (Address (Address))
import Plutus.V1.Ledger.Contexts (
  ScriptContext (ScriptContext),
  ScriptPurpose (Certifying, Minting, Rewarding, Spending),
  TxInInfo (TxInInfo),
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
  TxOut (TxOut),
  TxOutRef (TxOutRef),
 )
import Plutus.V1.Ledger.Credential (
  Credential (PubKeyCredential, ScriptCredential),
  StakingCredential (StakingHash, StakingPtr),
 )
import Plutus.V1.Ledger.Crypto (PubKeyHash (PubKeyHash))
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
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import Plutus.V1.Ledger.Scripts (
  Datum (Datum),
  DatumHash (DatumHash),
  ValidatorHash (ValidatorHash),
 )
import Plutus.V1.Ledger.Time (POSIXTime (POSIXTime))
import Plutus.V1.Ledger.TxId (TxId (TxId))
import Plutus.V1.Ledger.Value (
  CurrencySymbol (CurrencySymbol),
  TokenName (TokenName),
  Value (Value),
 )
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (matchData)
import PlutusTx.Prelude qualified as PTx

-- | @since 2.1
data Skeleton
  = ByteStringS PTx.BuiltinByteString
  | BoolS Bool
  | IntegerS Integer
  | StringS PTx.BuiltinString
  | AssocMapS (AssocMap.Map Skeleton Skeleton)
  | ConS PTx.BuiltinString [Skeleton]
  | RecS
      PTx.BuiltinString
      (PTx.BuiltinString, Skeleton)
      [(PTx.BuiltinString, Skeleton)]
  | TupleS Skeleton [Skeleton]
  | ListS [Skeleton]
  deriving stock
    ( -- | @since 2.1
      Eq
    , -- | @since 2.1
      Show
    )

-- | @since 2.1
instance PTx.Eq Skeleton where
  {-# INLINEABLE (==) #-}
  sk == sk' = case (sk, sk') of
    (BoolS b, BoolS b') -> b PTx.== b'
    (IntegerS i, IntegerS i') -> i PTx.== i'
    (ByteStringS bs, ByteStringS bs') -> bs PTx.== bs'
    (StringS s, StringS s') -> s PTx.== s'
    (AssocMapS m, AssocMapS m') -> m PTx.== m'
    (ConS nam sks, ConS nam' sks') ->
      nam PTx.== nam' PTx.&& sks PTx.== sks'
    (RecS nam keyVal keyVals, RecS nam' keyVal' keyVals') ->
      keyVal PTx.== keyVal'
        PTx.&& keyVals PTx.== keyVals'
        PTx.&& nam PTx.== nam'
    (TupleS x xs, TupleS x' xs') ->
      x PTx.== x' PTx.&& xs PTx.== xs'
    (ListS xs, ListS xs') -> xs PTx.== xs'
    _ -> False

{- | @since 2.1

 Instance must define a representable functor, that is, x == y iff skeletize
 x == skeletize y
-}
class (PTx.Eq a) => Skeletal a where
  skeletize :: a -> Skeleton

-- | @since 2.1
instance Skeletal PTx.BuiltinData where
  {-# INLINEABLE skeletize #-}
  skeletize dat = matchData dat mkConstr mkMap mkList mkI mkB
    where
      mkConstr :: Integer -> [PTx.BuiltinData] -> Skeleton
      mkConstr ix =
        ConS ("Constr " PTx.<> (fromString . show $ ix)) . PTx.fmap skeletize
      mkMap :: [(PTx.BuiltinData, PTx.BuiltinData)] -> Skeleton
      mkMap = ConS "Map" . (: []) . ListS . PTx.fmap skeletize
      mkList :: [PTx.BuiltinData] -> Skeleton
      mkList = ConS "List" . (: []) . ListS . PTx.fmap skeletize
      mkI :: Integer -> Skeleton
      mkI = ConS "I" . (: []) . skeletize
      mkB :: PTx.BuiltinByteString -> Skeleton
      mkB = ConS "B" . (: []) . skeletize

-- | @since 2.1
instance Skeletal PTx.BuiltinString where
  {-# INLINEABLE skeletize #-}
  skeletize = StringS

-- | @since 2.1
instance Skeletal Integer where
  {-# INLINEABLE skeletize #-}
  skeletize = IntegerS

-- | @since 2.1
instance Skeletal Bool where
  {-# INLINEABLE skeletize #-}
  skeletize = BoolS

-- | @since 2.1
instance Skeletal PTx.BuiltinByteString where
  {-# INLINEABLE skeletize #-}
  skeletize = ByteStringS

-- | @since 2.1
instance (Skeletal a) => Skeletal (Maybe a) where
  {-# INLINEABLE skeletize #-}
  skeletize = \case
    Nothing -> ConS "Nothing" []
    Just x -> ConS "Just" [skeletize x]

-- | @since 2.1
instance (Skeletal a) => Skeletal [a] where
  {-# INLINEABLE skeletize #-}
  skeletize = ListS . PTx.fmap skeletize

-- | @since 2.1
instance (Skeletal k, Skeletal v) => Skeletal (AssocMap.Map k v) where
  {-# INLINEABLE skeletize #-}
  skeletize = AssocMapS . AssocMap.fromList . PTx.fmap go . AssocMap.toList
    where
      go :: (k, v) -> (Skeleton, Skeleton)
      go (key, val) = (skeletize key, skeletize val)

-- | @since 2.1
instance (Skeletal a, Skeletal b) => Skeletal (a, b) where
  {-# INLINEABLE skeletize #-}
  skeletize (x, y) = TupleS (skeletize x) [skeletize y]

-- | @since 2.1
instance Skeletal TxId where
  {-# INLINEABLE skeletize #-}
  skeletize (TxId bbs) = ConS "TxId" [skeletize bbs]

-- | @since 2.1
instance Skeletal TxOutRef where
  {-# INLINEABLE skeletize #-}
  skeletize (TxOutRef txi txix) =
    RecS
      "TxOutRef"
      ("txOutRefId", skeletize txi)
      [("txOutRefIdx", skeletize txix)]

-- | @since 2.1
instance Skeletal PubKeyHash where
  {-# INLINEABLE skeletize #-}
  skeletize (PubKeyHash bbs) = ConS "PubKeyHash" [skeletize bbs]

-- | @since 2.1
instance Skeletal ValidatorHash where
  {-# INLINEABLE skeletize #-}
  skeletize (ValidatorHash bbs) = ConS "ValidatorHash" [skeletize bbs]

-- | @since 2.1
instance Skeletal Credential where
  {-# INLINEABLE skeletize #-}
  skeletize = \case
    PubKeyCredential pkh -> ConS "PubKeyCredential" [skeletize pkh]
    ScriptCredential vh -> ConS "ScriptCredential" [skeletize vh]

-- | @since 2.1
instance Skeletal StakingCredential where
  {-# INLINEABLE skeletize #-}
  skeletize = \case
    StakingHash cred -> ConS "StakingHash" [skeletize cred]
    StakingPtr i j k -> ConS "StakingPtr" . fmap skeletize $ [i, j, k]

-- | @since 2.1
instance Skeletal Address where
  {-# INLINEABLE skeletize #-}
  skeletize (Address cred stakingCred) =
    RecS
      "Address"
      ("addressCredential", skeletize cred)
      [("addressStakingCredential", skeletize stakingCred)]

-- | @since 2.1
instance Skeletal CurrencySymbol where
  {-# INLINEABLE skeletize #-}
  skeletize (CurrencySymbol bbs) = ConS "CurrencySymbol" [skeletize bbs]

-- | @since 2.1
instance Skeletal TokenName where
  {-# INLINEABLE skeletize #-}
  skeletize (TokenName bbs) = ConS "TokenName" [skeletize bbs]

-- | @since 2.1
instance Skeletal Value where
  {-# INLINEABLE skeletize #-}
  skeletize (Value x) = ConS "Value" [skeletize x]

-- | @since 2.1
instance Skeletal DatumHash where
  {-# INLINEABLE skeletize #-}
  skeletize (DatumHash bbs) = ConS "DatumHash" [skeletize bbs]

-- | @since 2.1
instance Skeletal TxOut where
  {-# INLINEABLE skeletize #-}
  skeletize (TxOut addr val mHash) =
    RecS
      "TxOut"
      ("txOutAddress", skeletize addr)
      [ ("txOutValue", skeletize val)
      , ("txOutDatumHash", skeletize mHash)
      ]

-- | @since 2.1
instance Skeletal TxInInfo where
  {-# INLINEABLE skeletize #-}
  skeletize (TxInInfo txoRef txo) =
    RecS
      "TxInInfo"
      ("txInInfoOutRef", skeletize txoRef)
      [("txInInfoResolved", skeletize txo)]

-- | @since 2.1
instance Skeletal DCert where
  {-# INLINEABLE skeletize #-}
  skeletize = \case
    DCertDelegRegKey sc -> ConS "DCertDelegRegKey" [skeletize sc]
    DCertDelegDeRegKey sc -> ConS "DCertDelegDeRegKey" [skeletize sc]
    DCertDelegDelegate sc pkh ->
      ConS "DCertDelegDelegate" [skeletize sc, skeletize pkh]
    DCertPoolRegister pkh vfr ->
      ConS "DCertPoolRegister" [skeletize pkh, skeletize vfr]
    DCertPoolRetire pkh i ->
      ConS "DCertPoolRetire" [skeletize pkh, skeletize i]
    DCertGenesis -> ConS "DCertGenesis" []
    DCertMir -> ConS "DCertMir" []

-- | @since 2.1
instance (Skeletal a) => Skeletal (Extended a) where
  {-# INLINEABLE skeletize #-}
  skeletize = \case
    NegInf -> ConS "NegInf" []
    Finite x -> ConS "Finite" [skeletize x]
    PosInf -> ConS "PosInf" []

-- | @since 2.1
instance (Skeletal a) => Skeletal (LowerBound a) where
  {-# INLINEABLE skeletize #-}
  skeletize (LowerBound ext clos) =
    ConS "LowerBound" [skeletize ext, skeletize clos]

-- | @since 2.1
instance (Skeletal a) => Skeletal (UpperBound a) where
  {-# INLINEABLE skeletize #-}
  skeletize (UpperBound ext clos) =
    ConS "UpperBound" [skeletize ext, skeletize clos]

-- | @since 2.1
instance (Skeletal a) => Skeletal (Interval a) where
  {-# INLINEABLE skeletize #-}
  skeletize (Interval from to) =
    RecS
      "Interval"
      ("ivFrom", skeletize from)
      [("ivTo", skeletize to)]

-- | @since 2.1
instance Skeletal POSIXTime where
  {-# INLINEABLE skeletize #-}
  skeletize (POSIXTime i) = ConS "POSIXTime" [skeletize i]

-- | @since 2.1
instance Skeletal Datum where
  {-# INLINEABLE skeletize #-}
  skeletize (Datum bd) = ConS "Datum" [skeletize bd]

-- | @since 2.1
instance Skeletal TxInfo where
  {-# INLINEABLE skeletize #-}
  skeletize txi =
    RecS "TxInfo" ("txInfoInputs", skeletize . txInfoInputs $ txi) go
    where
      go :: [(PTx.BuiltinString, Skeleton)]
      go =
        [ ("txInfoOutputs", skeletize . txInfoOutputs $ txi)
        , ("txInfoFee", skeletize . txInfoFee $ txi)
        , ("txInfoMint", skeletize . txInfoMint $ txi)
        , ("txInfoDCert", skeletize . txInfoDCert $ txi)
        , ("txInfoWdrl", skeletize . txInfoWdrl $ txi)
        , ("txInfoValidRange", skeletize . txInfoValidRange $ txi)
        , ("txInfoSignatories", skeletize . txInfoSignatories $ txi)
        , ("txInfoData", skeletize . txInfoData $ txi)
        , ("txInfoId", skeletize . txInfoId $ txi)
        ]

-- | @since 2.1
instance Skeletal ScriptPurpose where
  {-# INLINEABLE skeletize #-}
  skeletize = \case
    Minting cs -> ConS "Minting" [skeletize cs]
    Spending tor -> ConS "Spending" [skeletize tor]
    Rewarding sc -> ConS "Rewarding" [skeletize sc]
    Certifying dcert -> ConS "Certifying" [skeletize dcert]

-- | @since 2.1
instance Skeletal ScriptContext where
  {-# INLINEABLE skeletize #-}
  skeletize (ScriptContext txi sp) =
    RecS
      "ScriptContext"
      ("scriptContextTxInfo", skeletize txi)
      [("scriptContextPurpose", skeletize sp)]

{-# LANGUAGE TemplateHaskell #-}

module PlutusTx.AsData.Internal.TH (mkAsDataAccessors) where

import Language.Haskell.TH (
  Body (NormalB),
  Clause (Clause),
  Dec (FunD, PragmaD, SigD),
  Exp (VarE),
  Inline (Inlinable),
  Name,
  Pat (ListP, VarP, WildP),
  Phases (AllPhases),
  Pragma (InlineP),
  Q,
  RuleMatch (FunLike),
  Type (ConT),
  mkName,
  nameBase,
  newName,
 )
import Language.Haskell.TH.Datatype (
  ConstructorInfo (constructorFields, constructorVariant),
  ConstructorVariant (RecordConstructor),
  DatatypeInfo (datatypeCons),
  reifyDatatype,
 )
import Plutus.V1.Ledger.Api (BuiltinData (BuiltinData), Data (Constr))
import PlutusTx.AsData.Internal (AsData (AsData))
import PlutusTx.Prelude qualified as P

mkAsDataAccessors :: Name -> Q [Dec]
mkAsDataAccessors dtName = do
  dtInfo <- reifyDatatype dtName
  case datatypeCons dtInfo of
    [singleCon]
      | RecordConstructor fieldNames <- constructorVariant singleCon ->
        let fieldTypes = constructorFields singleCon
            fields = zip3 [0 ..] fieldNames fieldTypes
            size = length fields
         in concat <$> traverse (genSingleAccessor (ConT dtName) size) fields
    _ -> pure []

genSingleAccessor :: Type -> Int -> (Int, Name, Type) -> Q [Dec]
genSingleAccessor dt size (pos, fieldName, ty) = do
  let name = mkName $ nameBase fieldName
  let inlineable = PragmaD (InlineP name Inlinable FunLike AllPhases)
  funTy <- [t|AsData $(pure dt) -> AsData $(pure ty)|]
  let sig = SigD name funTy
  x <- newName "x"
  pat <- [p|AsData (BuiltinData (Constr 0 $(pure $ ListP $ makePats x size pos)))|]
  ex <- [|AsData (BuiltinData $(pure $ VarE x))|]
  let normalClause = Clause [pat] (NormalB ex) []
  err <- [|P.traceError "AsData: malformed AsData value."|]
  let impossibleClause = Clause [WildP] (NormalB err) []
  let dec = FunD name [normalClause, impossibleClause]
  pure [inlineable, sig, dec]
  where
    makePats :: Name -> Int -> Int -> [Pat]
    makePats _ 0 _ = []
    makePats x s p
      | p == 0 = VarP x : makePats x (s - 1) (p - 1)
      | otherwise = WildP : makePats x (s - 1) (p - 1)

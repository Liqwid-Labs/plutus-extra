{-# LANGUAGE TemplateHaskell #-}

module PlutusTx.Skeleton.QQ (makeSkeletal) where

import Control.Monad (replicateM)
import GHC.Exts (fromString)
import Language.Haskell.TH (
  Bang,
  Body (NormalB),
  Clause (Clause),
  Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
  Dec (DataD, FunD, InstanceD, NewtypeD, PragmaD),
  Exp (AppE, CaseE, ConE, ListE, LitE, TupE, VarE),
  Info (TyConI),
  Inline (Inlinable),
  Lit (StringL),
  Match (Match),
  Name,
  Pat (ConP, VarP),
  Phases (AllPhases),
  Pragma (InlineP),
  Q,
  RuleMatch (FunLike),
  TyVarBndr (KindedTV, PlainTV),
  Type (AppT, ConT, VarT),
  nameBase,
  newName,
  reify,
 )
import PlutusTx.Skeleton.Internal (
  Skeletal (skeletize),
  Skeleton (ConS, RecS),
 )

makeSkeletal :: Name -> Q [Dec]
makeSkeletal name = do
  info <- reify name
  case info of
    TyConI (DataD _ name' tyVars _ constructors _) ->
      mkSkeletal name' tyVars constructors
    TyConI (NewtypeD _ name' tyVars _ constructor _) ->
      mkSkeletal name' tyVars [constructor]
    _ -> error $ nameBase name <> " is not a data or newtype-defined type."

-- Helpers

mkSkeletal :: Name -> [TyVarBndr] -> [Con] -> Q [Dec]
mkSkeletal name tyVars constructors = do
  let namePreds = mkCtxVar <$> tyVars
  let instanceType = mkInstanceType name (fst <$> namePreds)
  method <- mkSkeletalMethod constructors
  pure [InstanceD Nothing (snd <$> namePreds) instanceType method]

mkCtxVar :: TyVarBndr -> (Name, Type)
mkCtxVar = \case
  PlainTV name -> (name, go name)
  KindedTV name _ -> (name, go name)
  where
    go :: Name -> Type
    go = AppT (ConT ''Skeletal) . VarT

mkInstanceType :: Name -> [Name] -> Type
mkInstanceType typeName = AppT (ConT ''Skeletal) . foldr go (ConT typeName)
  where
    go :: Name -> Type -> Type
    go varName acc = AppT acc (VarT varName)

mkSkeletalMethod :: [Con] -> Q [Dec]
mkSkeletalMethod constructors = do
  inputVarName <- newName "x"
  let varPat = VarP inputVarName
  let varExp = VarE inputVarName
  let methodInlinable = PragmaD . InlineP 'skeletize Inlinable FunLike $ AllPhases
  funDef <-
    FunD 'skeletize <$> case constructors of
      [] -> error "Cannot generate Skeletal for a type with no constructors."
      _ -> do
        bod <- NormalB . CaseE varExp <$> traverse mkSkeletalMatch constructors
        pure [Clause [varPat] bod []]
  pure [methodInlinable, funDef]

mkSkeletalMatch :: Con -> Q Match
mkSkeletalMatch = \case
  NormalC name vars -> do
    let argCount = length vars
    names <- replicateM argCount (newName "x")
    let matchPat = ConP name . fmap VarP $ names
    let bod = NormalB . mkConstr name $ names
    pure . Match matchPat bod $ []
  RecC name vars -> do
    fieldVarNames <- traverse mkFieldVarName vars
    let matchPat = ConP name . fmap (VarP . snd) $ fieldVarNames
    let bod = NormalB . mkRec name $ fieldVarNames
    pure . Match matchPat bod $ []
  InfixC {} ->
    error "Cannot generate Skeletal for types with infix constructors."
  ForallC {} ->
    error "Cannot generate Skeletal for types with existentials."
  GadtC {} ->
    error "Cannot generate Skeletal for GADTs."
  RecGadtC {} ->
    error "Cannot generate Skeletal for GADTs."

mkFieldVarName :: (Name, Bang, Type) -> Q (Name, Name)
mkFieldVarName (fieldName, _, _) = (fieldName,) <$> newName "x"

mkConstr :: Name -> [Name] -> Exp
mkConstr conName =
  AppE (AppE (ConE 'ConS) (builtinStringLit conName))
    . ListE
    . fmap skeletizeVar

mkRec :: Name -> [(Name, Name)] -> Exp
mkRec recConName = \case
  [] -> AppE (recApp 'ConS) . ListE $ []
  ((fieldName, varName) : fieldVarNames) ->
    AppE
      ( AppE
          (recApp 'RecS)
          ( TupE
              [ Just . builtinStringLit $ fieldName
              , Just . skeletizeVar $ varName
              ]
          )
      )
      . ListE
      . fmap go
      $ fieldVarNames
  where
    recApp :: Name -> Exp
    recApp name = AppE (ConE name) (builtinStringLit recConName)
    go :: (Name, Name) -> Exp
    go (fieldName, varName) =
      TupE
        [ Just . builtinStringLit $ fieldName
        , Just . skeletizeVar $ varName
        ]

builtinStringLit :: Name -> Exp
builtinStringLit = AppE (VarE 'fromString) . LitE . StringL . nameBase

skeletizeVar :: Name -> Exp
skeletizeVar = AppE (VarE 'skeletize) . VarE

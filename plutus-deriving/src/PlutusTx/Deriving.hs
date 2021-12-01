{-# LANGUAGE TemplateHaskell #-}

module PlutusTx.Deriving (deriveEq) where

import Control.Monad (replicateM)
import Language.Haskell.TH (
  Body (NormalB),
  Clause (Clause),
  Con (
    ForallC,
    GadtC,
    InfixC,
    NormalC,
    RecC,
    RecGadtC
  ),
  Dec (
    DataD,
    FunD,
    InstanceD,
    NewtypeD,
    PragmaD
  ),
  Exp (ConE, UInfixE, VarE),
  Info (TyConI),
  Inline (Inlinable),
  Name,
  Pat (ConP, VarP, WildP),
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
import PlutusTx.Prelude qualified as PTx

{- | Quick how to use:

 > {\-# LANGUAGE TemplateHaskell #-\}
 >
 > data Foo = ...
 >
 > $(deriveEq ''Foo)

 If something weird happens, you can inspect the output using @{\-# OPTIONS_GHC
 -ddump-splices #-\}@ in the source file. This will dump what TH made.

 @since 1.0
-}
deriveEq :: Name -> Q [Dec]
deriveEq name = do
  info <- reify name
  case info of
    TyConI (DataD _ name' tyVars _ constructors _) ->
      mkEq name' tyVars constructors
    TyConI (NewtypeD _ name' tyVars _ constructor _) ->
      mkEq name' tyVars [constructor]
    _ -> error $ nameBase name <> " is not a data or newtype-defined type."

-- Helpers

mkEq :: Name -> [TyVarBndr] -> [Con] -> Q [Dec]
mkEq name tyVars constructors = do
  let namePreds = mkCtxVar <$> tyVars
  let instanceType = mkInstanceType name (fst <$> namePreds)
  method <- mkEqMethod constructors
  pure [InstanceD Nothing (snd <$> namePreds) instanceType method]

mkCtxVar :: TyVarBndr -> (Name, Type)
mkCtxVar = \case
  PlainTV name -> (name, go name)
  KindedTV name _ -> (name, go name)
  where
    go :: Name -> Type
    go = AppT (ConT ''PTx.Eq) . VarT

mkInstanceType :: Name -> [Name] -> Type
mkInstanceType typeName = AppT (ConT ''PTx.Eq) . foldr go (ConT typeName)
  where
    go :: Name -> Type -> Type
    go varName acc = AppT acc (VarT varName)

mkEqMethod :: [Con] -> Q [Dec]
mkEqMethod constructors = do
  let methodInlineable = PragmaD . InlineP '(PTx.==) Inlinable FunLike $ AllPhases
  funDef <-
    FunD '(PTx.==) <$> case constructors of
      [] -> error "Cannot generate Eq for a type with no constructors."
      _ -> do
        activeClauses <- traverse mkConstructorMatch constructors
        let catchAllClause =
              Clause
                [WildP, WildP]
                (NormalB . ConE $ 'PTx.False)
                []
        pure $ activeClauses <> [catchAllClause]
  pure [methodInlineable, funDef]

mkConstructorMatch :: Con -> Q Clause
mkConstructorMatch = \case
  NormalC name vars -> go name . length $ vars
  RecC name vars -> go name . length $ vars
  InfixC {} ->
    error "Cannot generate Eq for types with infix constructors."
  ForallC {} ->
    error "Cannot generate Eq for types with existentials."
  GadtC {} ->
    error "Cannot generate Eq for GADTs."
  RecGadtC {} ->
    error "Cannot generate Eq for GADTs."
  where
    go :: Name -> Int -> Q Clause
    go name count = do
      namesLeft <- replicateM count (newName "x")
      namesRight <- replicateM count (newName "y")
      let leftPat = ConP name . fmap VarP $ namesLeft
      let rightPat = ConP name . fmap VarP $ namesRight
      let bod = NormalB $ case zip namesLeft namesRight of
            [] -> ConE 'PTx.True
            (lName, rName) : names ->
              foldr
                andEq
                (UInfixE (VarE lName) (VarE '(PTx.==)) (VarE rName))
                names
      pure . Clause [leftPat, rightPat] bod $ []

andEq :: (Name, Name) -> Exp -> Exp
andEq (lName, rName) =
  UInfixE (UInfixE (VarE lName) (VarE '(PTx.==)) (VarE rName)) (VarE '(PTx.&&))

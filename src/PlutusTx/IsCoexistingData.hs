{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Provides a TH-based generator of 'IsData' instances without the problem of
 identically-named constructors clashing on representation.

 = The problem

 Consider the following code:

 > data ABC = A | B | C
 >
 > data D = D

 The provided TH-based solution in Plutus for deriving 'IsData' for these
 types produces a \'tagged\' representation. This would yield the following
 representation for values of type @ABC@:

 * @toData A = Constr 0 []@
 * @toData B = Constr 1 []@
 * @toData C = Constr 2 []@

 Whereas for type @D@, we would get:

 * @toData D = Constr 0 []@

 We note that @A@ and @D@ have identical representations, despite being values
 of different types. This can cause issues when decoding from 'Data' values
 (which are effectively untyped), with basically no warning. Historically,
 this has caused us considerable trouble.

 = The solution

 We create a tagged approach to construction of 'Data' representations. We do
 this by computing a representation of a type's fully-qualified name
 (including its module of origin) as an 'Integer'. Currently, we convert the
 ASCII representation of the fully-qualified name directly (bytes-to-bytes),
 due to a limitation with ByteString literals (described in [this issue](https://github.com/input-output-hk/plutus/issues/3156)).

 For example, consider the following type:

 > module Quux
 > data Foo = Bar | Baz ByteString

 Suppose that the 'Integer' representation of @"Quux.Foo" is @123456@. @Bar@
 values would have a 'Data' representation like so:

 > Constr 0 [I 123456]

 while @Baz@ values would be:

 > Constr 1 [I 123456, B "whatever" ]

 The first element in the list field of the 'Data.Constr' constructor
 always corresponds to the \'tag\' based on @Foo@'s fully-qualified name. We
 use this when decoding using 'fromData': we first check if the \'tag\'
 corresponds to the expected tag for the type, and fail if not.

 = How to use this

 We use the @Foo@ example from above throughout. With Template Haskell enabled,
 use the function from this module to automatically construct the 'IsData'
 instance:

 > {\-# LANGUAGE TemplateHaskell #-\}
 >
 > module Quux
 >
 > import PlutusTx.IsCoexistingData (makeCoexistingIsData)
 >
 > data Foo = Bar | Baz ByteString
 >
 > makeCoexistingIsData ''Foo

 = Troubleshooting

 == Error messages about missing definitions

 Sometimes, use of 'makeCoexistingIsData' can cause interference with other TH,
 such as what is used by @tasty@ for @TestTree@s. In such cases, invoke
 'makeCoexistingIsData' at the bottom of the module.

 == Errors from Plutus regarding @traverse@

 This occurs when a data type for which we use 'makeCoexistingIsData' contains
 fields which are lists, or anything based on lists (which includes @AssocMap@).
 In this case, add @{\-# OPTIONS_GHC -fno-specialize #-\}@ at the top of the
 file where 'makeCoexistingIsData' is used. If this still doesn't solve the
 issue, also add @{\-# OPTIONS_GHC -fno-strictness #-\}@.
-}
module PlutusTx.IsCoexistingData (makeCoexistingIsData) where

import Control.Monad (replicateM)
import Data.Char (ord)
import Data.Foldable (foldl')
import Instances.TH.Lift ()
import Language.Haskell.TH (
  Body (GuardedB, NormalB),
  Clause (Clause),
  Dec (FunD, InstanceD, PragmaD),
  Exp (AppE, CaseE, ConE, ListE, UInfixE, VarE),
  Guard (NormalG),
  Inline (Inlinable),
  Match (Match),
  Name,
  Pat (ConP, InfixP, ListP, VarP, WildP),
  Phases (AllPhases),
  Pragma (InlineP),
  Q,
  RuleMatch (FunLike),
  TyVarBndr (KindedTV, PlainTV),
  Type (AppT, ConT, VarT),
  integerL,
  litE,
  newName,
  varE,
 )
import Language.Haskell.TH.Datatype (
  ConstructorInfo,
  constructorFields,
  constructorName,
  datatypeCons,
  datatypeVars,
  reifyDatatype,
 )
import Language.Haskell.TH.Syntax (lift)
import PlutusTx.Data qualified as Data
import PlutusTx.IsData.Class (IsData (fromData, toData))
import PlutusTx.Prelude hiding (
  fmap,
  length,
  pure,
  sequence,
  traverse,
  (<$>),
  (<*>),
 )
import PlutusTx.Prelude qualified as PTx
import Prelude (
  fmap,
  length,
  pure,
  traverse,
  (<$>),
  (<*>),
 )
import Prelude qualified

-- | Template Haskell helper to generate a \'tagging\' 'IsData' representation.
makeCoexistingIsData :: Name -> Q [Dec]
makeCoexistingIsData typeName = do
  -- Generate a unique code representing our type.
  let !typeCode =
        sum
          . fmap (Prelude.uncurry toOffsetInteger)
          . zip [0 ..]
          . Prelude.show
          $ typeName
  -- Get full data type info.
  info <- reifyDatatype typeName
  -- Fetch a list of all the type parameters our data type has.
  let typeParams = datatypeVars info
  -- Generate an IsData constraint for each of them.
  let prereqs = mkPrereqs typeParams
  -- Build IsData signature, including all of our type parameters.
  --
  -- So something like:
  --
  -- IsData (Foo a b c d) where
  let signature = mkSignature typeName typeParams
  -- Fetch an indexed list of all data constructors.
  let constructors = zip [0 ..] . datatypeCons $ info
  -- Generate implementations of toData and fromData.
  -- This is complicated by two things:
  --
  -- 1. We have to have INLINEABLE pragmata on both to play nice with on-chain;
  --    and
  -- 2. Because of restrictions on-chain, we can't rely on (==) on Integer
  --    behaving itself, including for internal 'if'-expressions and nested
  --    matches.
  --
  -- In particular, 2 requires us to have a form like this for a fromData
  -- definition:
  --
  -- fromData (Constr n (I t : ds))
  --   | t == 1234 && n == 0 = case ds of ...
  --   | t == 1234 && n == 1 = case ds of ...
  --   ...
  -- fromData _ = Nothing
  --
  -- because currently, nothing else will work the way we expect.
  methods <- mkMethods constructors typeCode
  -- Assemble everything.
  pure [InstanceD Nothing prereqs signature methods]

-- Helpers

{-# INLINE mkMethods #-}
mkMethods :: [(Integer, ConstructorInfo)] -> Integer -> Q [Dec]
mkMethods constructors typeCode = do
  -- Fresh variable representing the toData argument.
  toDataVar <- newName "x"
  -- Define our INLINEABLE pragmata.
  let pragmata = fmap mkInlinable ['toData, 'fromData]
  -- ... and implementations.
  toDataDef <- FunD 'toData <$> mkToDataDef toDataVar typeCode constructors
  fromDataDef <-
    FunD 'fromData <$> mkFromDataDef typeCode constructors
  pure $ pragmata <> [toDataDef, fromDataDef]

-- Generates the equivalent of:
--
-- case var of
--  constructor1 a1 a2 ... an -> Constr 0 [I tag, toData a1, ... toData an]
--  constructor2 a1 a2 ... am -> Constr 1 [I tag, toData a1, ... toData am]
--  ...
{-# INLINE mkToDataDef #-}
mkToDataDef :: Name -> Integer -> [(Integer, ConstructorInfo)] -> Q [Clause]
mkToDataDef var code constructors = do
  res <- mkNormalClause var <$> mkToDataCases
  pure [res]
  where
    -- Construct the overall case statement.
    mkToDataCases :: Q Exp
    mkToDataCases =
      CaseE (VarE var) <$> traverse (Prelude.uncurry go) constructors
    -- Take each tagged constructor info, and build the corresponding pattern
    -- match and Constr call.
    go :: Integer -> ConstructorInfo -> Q Match
    go ix info = do
      let len = length . constructorFields $ info
      matchNams <- replicateM len . newName $ "a"
      let patMatch = ConP (constructorName info) (fmap VarP matchNams)
      constr <- mkConstr ix matchNams
      pure . Match patMatch (NormalB constr) $ []
    -- Make a Constr call, placing the tag inside the first data argument.
    mkConstr :: Integer -> [Name] -> Q Exp
    mkConstr ix matchNams = do
      let litIx = litE . integerL $ ix
      [e|Data.Constr $litIx (Data.I $(lift code) : $(varData matchNams))|]
    -- Helper to build our list of fresh names into a list of fresh var
    -- expressions.
    varData :: [Name] -> Q Exp
    varData nams = ListE <$> traverse (\nam -> [e|toData $(varE nam)|]) nams

-- Generates the equivalent of
--
-- fromData (Constr n (I t : ds))
--   | t == code && n == 0 = case ds of ...
--   | t == code && n == 1 = case ds of ...
--   ...
-- fromData _ = Nothing
{-# INLINE mkFromDataDef #-}
mkFromDataDef :: Integer -> [(Integer, ConstructorInfo)] -> Q [Clause]
mkFromDataDef code constructors = do
  -- Fresh variable name for the constructor tag.
  tagVar <- newName "n"
  -- As above, but for the type code.
  codeVar <- newName "t"
  -- As above, but for the rest of the data.
  datasVar <- newName "ds"
  -- Construct 'Constr n (I t : ds)' using the names.
  let datasPat = InfixP (ConP 'Data.I [VarP codeVar]) '(:) (VarP datasVar)
  let constrPat = ConP 'Data.Constr [VarP tagVar, datasPat]
  guards <- traverse (go codeVar tagVar datasVar) constructors
  let constrMatch = Clause [constrPat] (GuardedB guards) []
  -- Construct 'fromData _ = Nothing'.
  fallbackClause <- Clause [WildP] <$> (NormalB <$> [e|Nothing|]) <*> pure []
  pure [constrMatch, fallbackClause]
  where
    -- Generate each '| t == code && n == ix = case ds of ...
    go :: Name -> Name -> Name -> (Integer, ConstructorInfo) -> Q (Guard, Exp)
    go codeVar tagVar datasVar (ix, info) = do
      eqGuard <-
        NormalG <$> [e|$(varE codeVar) == $(lift code) && $(varE tagVar) == $(litE . integerL $ ix)|]
      dsCase <- CaseE (VarE datasVar) <$> mkFromDataMatch info
      pure (eqGuard, dsCase)

{-# INLINE mkFromDataMatch #-}
mkFromDataMatch :: ConstructorInfo -> Q [Match]
mkFromDataMatch info = do
  -- Generate the 'catch-all' with Nothing.
  wildMatch <- Match WildP <$> (NormalB <$> [e|Nothing|]) <*> pure []
  -- Grab all the fields of the constructor in question.
  let fields = constructorFields info
  -- Make a list of fresh names for each of the fields.
  fieldVarNames <- replicateM (length fields) (newName "a")
  -- Generate a match against the exact number of list entries as the
  -- constructor has fields.
  let matchPat = ListP . fmap VarP $ fieldVarNames
  let conMatch = Match matchPat (NormalB . conSpread $ fieldVarNames) []
  pure [conMatch, wildMatch]
  where
    -- Generates either:
    --
    -- pure Foo
    --
    -- or
    --
    -- Bar <$> fromField a1 <*> fromField a2 <*> ... <*> fromField an
    conSpread :: [Name] -> Exp
    conSpread = \case
      [] -> AppE (VarE 'PTx.pure) conCon
      (n : ns) ->
        foldl' go (UInfixE conCon (VarE '(PTx.<$>)) (appFromData n)) ns
    conCon :: Exp
    conCon = ConE . constructorName $ info
    go :: Exp -> Name -> Exp
    go acc = UInfixE acc (VarE '(PTx.<*>)) . appFromData
    appFromData :: Name -> Exp
    appFromData = AppE (VarE 'fromData) . VarE

{-# INLINE mkNormalClause #-}
mkNormalClause :: Name -> Exp -> Clause
mkNormalClause var def = Clause [VarP var] (NormalB def) []

{-# INLINE mkInlinable #-}
mkInlinable :: Name -> Dec
mkInlinable nam = PragmaD . InlineP nam Inlinable FunLike $ AllPhases

{-# INLINE mkSignature #-}
mkSignature :: Name -> [TyVarBndr] -> Type
mkSignature typeName = AppT (ConT ''IsData) . foldl' go (ConT typeName)
  where
    go :: Type -> TyVarBndr -> Type
    go acc = AppT acc . tyVar

{-# INLINE toOffsetInteger #-}
toOffsetInteger :: Prelude.Integer -> Prelude.Char -> Integer
toOffsetInteger i c =
  (Prelude.fromIntegral . ord $ c)
    * (256 Prelude.^ i)

{-# INLINE mkPrereqs #-}
mkPrereqs :: [TyVarBndr] -> [Type]
mkPrereqs = fmap (AppT (ConT ''IsData) . tyVar)

{-# INLINE tyVar #-}
tyVar :: TyVarBndr -> Type
tyVar =
  VarT . \case
    PlainTV nam -> nam
    KindedTV nam _ -> nam

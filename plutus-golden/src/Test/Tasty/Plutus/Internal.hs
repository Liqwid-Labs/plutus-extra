module Test.Tasty.Plutus.Internal (
  diffData,
  diffSchema,
  diffArgument,
  dataToExpr,
  schemaToExpr,
) where

import Data.Bifunctor (bimap)
import Data.Functor.Foldable (Fix, cata)
import Data.TreeDiff.Class (toExpr)
import Data.TreeDiff.Expr (
  Edit,
  EditExpr,
  Expr (App, Lst),
  exprDiff,
 )
import Plutus.V1.Ledger.Interval (
  Extended (Finite, NegInf, PosInf),
  Interval (ivFrom, ivTo),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import Plutus.V1.Ledger.Time (POSIXTime (getPOSIXTime))
import Plutus.V1.Ledger.Value (Value)
import PlutusTx (Data (B, Constr, I, List, Map))
import Schema (
  FormArgumentF (
    FormArrayF,
    FormBoolF,
    FormHexF,
    FormIntF,
    FormIntegerF,
    FormMaybeF,
    FormObjectF,
    FormPOSIXTimeRangeF,
    FormRadioF,
    FormStringF,
    FormTupleF,
    FormUnitF,
    FormUnsupportedF,
    FormValueF
  ),
  FormSchema (
    FormSchemaArray,
    FormSchemaBool,
    FormSchemaHex,
    FormSchemaInt,
    FormSchemaInteger,
    FormSchemaMaybe,
    FormSchemaObject,
    FormSchemaPOSIXTimeRange,
    FormSchemaRadio,
    FormSchemaString,
    FormSchemaTuple,
    FormSchemaUnit,
    FormSchemaUnsupported,
    FormSchemaValue
  ),
 )

diffData :: Data -> Data -> Edit EditExpr
diffData d1 d2 = exprDiff (dataToExpr d1) (dataToExpr d2)

dataToExpr :: Data -> Expr
dataToExpr = \case
  Constr ix args -> App ("Constr " <> show ix) . fmap dataToExpr $ args
  Map keyVals -> App "Map" [Lst . fmap (toExpr . bimap dataToExpr dataToExpr) $ keyVals]
  List xs -> Lst . fmap dataToExpr $ xs
  I i -> App "I" [toExpr i]
  B bs -> App "B" [toExpr bs]

diffSchema :: FormSchema -> FormSchema -> Edit EditExpr
diffSchema s1 s2 = exprDiff (schemaToExpr s1) (schemaToExpr s2)

schemaToExpr :: FormSchema -> Expr
schemaToExpr = \case
  FormSchemaUnit -> App "FormSchemaUnit" []
  FormSchemaBool -> App "FormSchemaBool" []
  FormSchemaInt -> App "FormSchemaInt" []
  FormSchemaInteger -> App "FormSchemaInteger" []
  FormSchemaString -> App "FormSchemaString" []
  FormSchemaHex -> App "FormSchemaHex" []
  FormSchemaArray scm -> App "FormSchemaArray" [schemaToExpr scm]
  FormSchemaMaybe scm -> App "FormSchemaMaybe" [schemaToExpr scm]
  FormSchemaRadio xs -> App "FormSchemaRadio" [Lst . fmap toExpr $ xs]
  FormSchemaTuple scm1 scm2 ->
    App "FormSchemaTuple" [schemaToExpr scm1, schemaToExpr scm2]
  FormSchemaObject keyVals ->
    App "FormSchemaObject" [Lst . fmap (toExpr . bimap toExpr schemaToExpr) $ keyVals]
  FormSchemaValue -> App "FormSchemaValue" []
  FormSchemaPOSIXTimeRange -> App "FormSchemaPOSIXTimeRange" []
  FormSchemaUnsupported s -> App "FormSchemaUnsupported" [toExpr s]

diffArgument :: Fix FormArgumentF -> Fix FormArgumentF -> Edit EditExpr
diffArgument fa1 fa2 = exprDiff (cata go fa1) (cata go fa2)
  where
    go :: FormArgumentF Expr -> Expr
    go = \case
      FormUnitF -> App "FormUnit" []
      FormBoolF b -> App "FormBool" [toExpr b]
      FormIntF i -> App "FormInt" [toExpr i]
      FormIntegerF i -> App "FormInteger" [toExpr i]
      FormStringF s -> App "FormString" [toExpr s]
      FormHexF hex -> App "FormHex" [toExpr hex]
      FormRadioF ss ms -> App "FormRadio" [toExpr ss, toExpr ms]
      FormArrayF scm exprs -> App "FormArray" [schemaToExpr scm, toExpr exprs]
      FormMaybeF scm ma -> App "FormMaybe" [schemaToExpr scm, toExpr ma]
      FormTupleF ex1 ex2 -> App "FormTuple" [ex1, ex2]
      FormObjectF keyVals -> App "FormObject" [toExpr keyVals]
      FormValueF v -> App "FormValue" [valueToExpr v]
      FormPOSIXTimeRangeF inter -> App "FormPOSIXTimeRange" [timeRangeToExpr inter]
      FormUnsupportedF s -> App "FormUnsupported" [toExpr s]

-- Helpers

valueToExpr :: Value -> Expr
valueToExpr _ = App "Value" []

timeRangeToExpr :: Interval POSIXTime -> Expr
timeRangeToExpr inter =
  App
    "Interval"
    [ lowerBoundToExpr . ivFrom $ inter
    , upperBoundToExpr . ivTo $ inter
    ]

lowerBoundToExpr :: LowerBound POSIXTime -> Expr
lowerBoundToExpr (LowerBound ext clos) =
  App "LowerBound" [extendedToExpr ext, toExpr clos]

upperBoundToExpr :: UpperBound POSIXTime -> Expr
upperBoundToExpr (UpperBound ext clos) =
  App "UpperBound" [extendedToExpr ext, toExpr clos]

extendedToExpr :: Extended POSIXTime -> Expr
extendedToExpr = \case
  NegInf -> App "NegInf" []
  Finite t -> App "Finite" [toExpr . getPOSIXTime $ t]
  PosInf -> App "PosInf" []

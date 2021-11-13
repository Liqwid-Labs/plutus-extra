module Test.Tasty.Plutus.Internal.Feedback (
  unexpectedFailure,
  scriptException,
  malformedScript,
  noOutcome,
  unexpectedSuccess,
  internalError,
  noParse,
  doPass,
  didn'tLog,
  dumpState,
  dumpState',
  ourStyle,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Plutus.V1.Ledger.Api (
  ScriptContext,
 )
import Plutus.V1.Pretty (scriptContextToValue)
import Test.Tasty.Plutus.Internal.Context (ContextBuilder, Purpose, TransactionConfig)
import Test.Tasty.Plutus.Internal.Env (getScriptContext)
import Test.Tasty.Plutus.Options (
  PlutusTracing (Always, OnlyOnFail),
 )
import Test.Tasty.Plutus.TestData (TestData (MintingTest, SpendingTest))
import Text.PrettyPrint (
  Doc,
  Style (lineLength),
  colon,
  hang,
  int,
  renderStyle,
  style,
  text,
  vcat,
  ($+$),
  (<+>),
  (<>),
 )
import Text.Show.Pretty (ppDoc, valToDoc)
import Prelude hiding ((<>))

ourStyle :: Style
ourStyle = style {lineLength = 80}

unexpectedFailure ::
  forall (a :: Type).
  (a -> Doc) ->
  String ->
  a ->
  String
unexpectedFailure getDumpedState msg env =
  renderStyle ourStyle $
    "Unexpected failure: " <+> text msg
      $+$ getDumpedState env

scriptException :: String -> String -> String
scriptException name msg =
  renderStyle ourStyle $
    "Unexpected behaviour in script:" <+> text name
      $+$ hang "Description" 4 (text msg)

malformedScript :: String -> String
malformedScript msg =
  renderStyle ourStyle $
    "Script was malformed"
      $+$ hang "Details" 4 (text msg)

noOutcome ::
  forall (a :: Type).
  (a -> Doc) ->
  a ->
  String
noOutcome getDumpedState env =
  renderStyle ourStyle $
    "No outcome from run"
      $+$ getDumpedState env
      $+$ ""
      $+$ "Did you forget to use toTestValidator or toTestMintingPolicy?"

unexpectedSuccess ::
  forall (a :: Type).
  (a -> Doc) ->
  a ->
  String
unexpectedSuccess getDumpedState env =
  renderStyle ourStyle $
    "Unexpected success" $+$ getDumpedState env

internalError ::
  forall (a :: Type).
  (a -> Doc) ->
  Text ->
  a ->
  String
internalError getDumpedState msg env = do
  renderStyle ourStyle $
    ("Internal error" <+> (text . show $ msg)) $+$ getDumpedState env

noParse ::
  forall (a :: Type).
  (a -> Doc) ->
  Text ->
  a ->
  String
noParse getDumpedState what env = do
  renderStyle ourStyle $
    ((text . show $ what) <+> "did not parse") $+$ getDumpedState env

doPass ::
  forall (a :: Type).
  (a -> PlutusTracing) ->
  [Text] ->
  a ->
  String
doPass getShouldChat logs env = case getShouldChat env of
  Always ->
    renderStyle ourStyle $
      ""
        $+$ hang "Logs" 4 (dumpLogs logs)
  OnlyOnFail -> ""

didn'tLog ::
  forall (a :: Type).
  (a -> Doc) ->
  a ->
  String
didn'tLog getDumpedState env =
  renderStyle ourStyle $
    "Trace did not contain expected contents"
      $+$ getDumpedState env

dumpState ::
  forall (a :: Type) (p :: Purpose).
  (a -> TransactionConfig) ->
  (a -> ContextBuilder p) ->
  (a -> TestData p) ->
  [Text] ->
  a ->
  Doc
dumpState getConf getCb getTd logs env =
  ""
    $+$ hang "Context" 4 (valToDoc . scriptContextToValue $ sc)
    $+$ hang "Configuration" 4 (ppDoc $ getConf env)
    $+$ hang "Inputs" 4 dumpInputs
    $+$ hang "Logs" 4 (dumpLogs logs)
  where
    sc :: ScriptContext
    sc = getScriptContext getConf getCb getTd env
    dumpInputs :: Doc
    dumpInputs = case getTd env of
      SpendingTest d r v ->
        "Datum"
          $+$ ppDoc d
          $+$ "Redeemer"
          $+$ ppDoc r
          $+$ "Value"
          $+$ ppDoc v
      MintingTest r ->
        "Redeemer" $+$ ppDoc r

dumpState' ::
  forall (a :: Type) (p :: Purpose).
  (a -> TransactionConfig) ->
  (a -> ContextBuilder p) ->
  (a -> TestData p) ->
  [Text] ->
  a ->
  Doc
dumpState' getConf getCb getTd logs env =
  ""
    $+$ hang "Context" 4 (valToDoc . scriptContextToValue $ sc)
    $+$ hang "Configuration" 4 (ppDoc $ getConf env)
    $+$ hang "Logs" 4 (dumpLogs logs)
  where
    sc :: ScriptContext
    sc = getScriptContext getConf getCb getTd env

dumpLogs :: [Text] -> Doc
dumpLogs = vcat . fmap go . zip [1 ..]
  where
    go :: (Int, Text) -> Doc
    go (ix, line) = (int ix <> colon) <+> (text . show $ line)

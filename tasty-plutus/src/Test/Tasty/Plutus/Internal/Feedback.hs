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
  errorNoEstimate,
  reportBudgets,
  explainFailureEstimation,
  explainFailureExhaustion,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Plutus.V1.Ledger.Api (
  ScriptContext,
 )
import Plutus.V1.Pretty (scriptContextToValue)
import Test.Plutus.ContextBuilder (ContextBuilder, Purpose, TransactionConfig)
import Test.Tasty.Plutus.Internal.Env (getScriptContext)
import Test.Tasty.Plutus.Options (
  PlutusTracing (Always, OnlyOnFail),
 )
import Test.Tasty.Plutus.TestData (TestData (MintingTest, SpendingTest))
import Text.PrettyPrint (
  Doc,
  Style (lineLength),
  colon,
  comma,
  hang,
  hcat,
  int,
  integer,
  punctuate,
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

explainFailureExhaustion :: String
explainFailureExhaustion =
  renderStyle ourStyle $
    "Gave up trying to generate a successful run for estimation."
      $+$ "This can happen due to what \'failure\' means for a script in tasty-plutus:"
      $+$ hang " * " 4 "The script ran successfully, but didn't log a success from its wrapper; or"
      $+$ hang " * " 4 "The script crashed or failed to evaluate."
      $+$ "Because of how the CEK evaluator for scripts works, we can only establish an estimate for the first case."
      $+$ "As property tests generate inputs pseudorandomly, even tests where failure is not guaranteed, if success isn't likely enough, we might never see an instrumentable case."
      $+$ "If you are seeing this message, this is exactly what happened."

explainFailureEstimation :: String
explainFailureEstimation =
  renderStyle ourStyle $
    "Cannot provide estimates for test cases whose scripts are designed to fail."
      $+$ "The reason for this relates to what \'failure\' can mean for a script in tasty-plutus:"
      $+$ hang " * " 4 "The script ran successfully, but didn't log a success from its wrapper; or"
      $+$ hang " * " 4 "The script crashed or failed to evaluate."
      $+$ "Because of how the CEK evaluator for scripts works, we can only establish an estimate for the first case."
      $+$ "Since we can't tell these cases apart in general, we provide no estimates in this situation."

reportBudgets :: Integer -> Integer -> String
reportBudgets bCPU bMem =
  renderStyle ourStyle $
    "Script resource estimates:"
      $+$ hang "CPU:" 4 cpuBudget
      $+$ hang "Memory:" 4 (memoryBudget <+> "machine words")
  where
    cpuBudget :: Doc
    cpuBudget =
      let chunked = thousandsChunks bCPU
       in case chunked of
            [nanos, picos] ->
              nanos <> "." <> picos <+> "nanoseconds"
            [micros, nanos, _] ->
              "~" <> micros <> "." <> nanos <+> "microseconds"
            [millis, micros, _, _] ->
              "~" <> millis <> "." <> micros <+> "milliseconds"
            [secs, millis, _, _, _] ->
              "~" <> secs <> "." <> millis <+> "seconds"
            -- default to comma-separated picos
            _ -> commaSep chunked <+> "picoseconds"
    memoryBudget :: Doc
    memoryBudget = commaSep . thousandsChunks $ bMem

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

errorNoEstimate :: [Text] -> String -> String
errorNoEstimate logs msg =
  renderStyle ourStyle $
    "Could not provide estimate due to script error"
      $+$ hang "Description:" 4 (text msg)
      $+$ hang "Logs:" 4 (dumpLogs logs)

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
      MintingTest r v ->
        "Redeemer"
          $+$ ppDoc r
          $+$ "Value"
          $+$ ppDoc v

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

-- Helpers

commaSep :: [Doc] -> Doc
commaSep = hcat . punctuate comma

thousandsChunks :: Integer -> [Doc]
thousandsChunks i = case quotRem i 1000 of
  (0, 0) -> []
  (0, r) -> [integer r]
  (d, r) -> thousandsChunks d ++ [integer r]

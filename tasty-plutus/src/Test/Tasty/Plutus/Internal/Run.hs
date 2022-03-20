module Test.Tasty.Plutus.Internal.Run (
  ScriptResult (..),
  testValidatorScript,
  testMintingPolicyScript,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Plutus.V1.Ledger.Api (
  Datum,
  MintingPolicy,
  Redeemer,
  Validator,
 )
import Plutus.V1.Ledger.Scripts (
  Context,
  ScriptError (EvaluationError, EvaluationException, MalformedScript),
  runMintingPolicyScript,
  runScript,
 )
import Safe (lastMay)

-- The result of parsing a log from a errored script emulation
data ScriptResult
  = ScriptFailed [Text] String
  | ParseFailed [Text] Text
  | PlutusEvaluationException String String
  | PlutusMalformedScript String
  deriving stock (Eq, Show)

testValidatorScript ::
  Context ->
  Validator ->
  Datum ->
  Redeemer ->
  Either ScriptResult [Text]
testValidatorScript ctx val d r = case runScript ctx val d r of
  Left err -> Left $ parseError err
  Right (_, logs) -> Right logs

testMintingPolicyScript ::
  Context ->
  MintingPolicy ->
  Redeemer ->
  Either ScriptResult [Text]
testMintingPolicyScript ctx mp r = case runMintingPolicyScript ctx mp r of
  Left err -> Left $ parseError err
  Right (_, logs) -> Right logs

parseError :: ScriptError -> ScriptResult
parseError = \case
  EvaluationError logs msg ->
    case lastMay logs >>= Text.stripPrefix "tasty-plutus: Parse failed: " of
      Nothing -> ScriptFailed logs msg
      Just t -> ParseFailed logs t
  EvaluationException name msg -> PlutusEvaluationException name msg
  MalformedScript msg -> PlutusMalformedScript msg

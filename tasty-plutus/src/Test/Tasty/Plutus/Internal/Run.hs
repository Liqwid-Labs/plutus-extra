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
  ScriptError,
  runMintingPolicyScript,
  runScript,
 )
import Safe (lastMay)

-- The result of parsing a log from a script emulation
data ScriptResult
  = ScriptPassed
  | ScriptFailed
  | NoOutcome
  | ParseFailed Text
  | InternalError Text
  deriving stock (Eq, Show)

testValidatorScript ::
  Context ->
  Validator ->
  Datum ->
  Redeemer ->
  Either ScriptError ([Text], ScriptResult)
testValidatorScript ctx val d r = case runScript ctx val d r of
  Left err -> Left err
  Right (_, logs) -> Right . (logs,) . parseLogs $ logs

testMintingPolicyScript ::
  Context ->
  MintingPolicy ->
  Redeemer ->
  Either ScriptError ([Text], ScriptResult)
testMintingPolicyScript ctx mp r = case runMintingPolicyScript ctx mp r of
  Left err -> Left err
  Right (_, logs) -> Right . (logs,) . parseLogs $ logs

parseLogs :: [Text] -> ScriptResult
parseLogs logs = case lastMay logs >>= Text.stripPrefix "tasty-plutus: " of
  Nothing -> NoOutcome
  Just "Pass" -> ScriptPassed
  Just "Fail" -> ScriptFailed
  Just t -> case Text.stripPrefix "Parse failed: " t of
    Nothing -> InternalError t
    Just t' -> ParseFailed t'

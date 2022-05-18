module Test.Tasty.Plutus.Instances (
  ResultJoin (ResultJoin, getResultJoin),
) where

import Data.Char qualified as Char
import Test.Tasty.Providers (
  testPassed,
 )
import Test.Tasty.Providers.ConsoleFormat (
  ResultDetailsPrinter (ResultDetailsPrinter),
 )
import Test.Tasty.Runners (
  Outcome (..),
  Result (..),
 )

newtype ResultJoin = ResultJoin {getResultJoin :: Result}

instance Semigroup ResultJoin where
  ResultJoin r1 <> ResultJoin r2 =
    ResultJoin $
      Result
        { resultOutcome = case (resultOutcome r1, resultOutcome r2) of
            (Success, Success) -> Success
            (Success, failure) -> failure
            (failure, _) -> failure
        , resultDescription = resultDescription r1 <+> resultDescription r2
        , resultShortDescription = resultShortDescription r1 <+> resultShortDescription r2
        , resultTime = resultTime r1 + resultTime r2
        , resultDetailsPrinter = ResultDetailsPrinter $ \indent printer ->
            let run Result {resultDetailsPrinter = ResultDetailsPrinter r} = r indent printer in run r1 *> run r2
        }

(<+>) :: String -> String -> String
a <+> b
  | all Char.isSpace a = b
  | all Char.isSpace b = a
  | a == b = a
  | '\n' `elem` (a <> b) = a <> "\n" <> b
  | otherwise = a <> " " <> b

instance Monoid ResultJoin where
  mempty = ResultJoin (testPassed "No tests to run")

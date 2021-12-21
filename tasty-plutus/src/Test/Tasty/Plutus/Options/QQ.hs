{-# LANGUAGE TemplateHaskell #-}

module Test.Tasty.Plutus.Options.QQ (
  testCount,
  maxSize,
) where

import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Language.Haskell.TH.Syntax (
  Dec,
  Exp (AppE, ConE, LitE),
  Lit (IntegerL),
  Pat,
  Q,
  Type,
 )
import Test.Tasty.Plutus.Internal.Options (
  PropertyMaxSize (PropertyMaxSize),
  PropertyTestCount (PropertyTestCount),
 )
import Text.Read (readMaybe)
import Prelude

{- | Construct a 'PropertyTestCount' from a positive integer literal.

 For technical reasons related to QuickCheck, we can only have positive
 literals that would fit into an 'Int'.

 An example of use:

 > barelyEnoughTests :: PropertyTestCount
 > barelyEnoughTests = [testCount| 1000 |]

 @since 3.1
-}
testCount :: QuasiQuoter
testCount =
  QuasiQuoter
    mkTestCount
    (noPattern "testCount")
    (noType "testCount")
    (noDecl "testCount")

{- | Construct a 'PropertyMaxSize' from a positive integer literal.

 For technical reasons related to QuickCheck, we can only have positive
 literals that would fit into an 'Int'.

 An example of use:

 > theDefaultSize :: PropertyMaxSize
 > theDefaultSize = [maxSize| 100 |]

 @since 3.1
-}
maxSize :: QuasiQuoter
maxSize =
  QuasiQuoter
    mkMaxSize
    (noPattern "maxSize")
    (noType "maxSize")
    (noDecl "maxSize")

-- Helpers

mkTestCount :: String -> Q Exp
mkTestCount s = case readMaybe s of
  Nothing -> parseFail s
  Just (i :: Int) -> case signum i of
    (-1) -> parseFail s
    0 -> parseFail s
    _ ->
      pure . AppE (ConE 'PropertyTestCount) . LitE . IntegerL . fromIntegral $ i

mkMaxSize :: String -> Q Exp
mkMaxSize s = case readMaybe s of
  Nothing -> parseFail s
  Just (i :: Int) -> case signum i of
    (-1) -> parseFail s
    0 -> parseFail s
    _ ->
      pure . AppE (ConE 'PropertyMaxSize) . LitE . IntegerL . fromIntegral $ i

parseFail :: String -> Q Exp
parseFail s = fail $ "Expected positive integer, instead got: " <> s

noPattern :: String -> String -> Q Pat
noPattern what _ = qError what "pattern"

noType :: String -> String -> Q Type
noType what _ = qError what "type"

noDecl :: String -> String -> Q [Dec]
noDecl what _ = qError what "declaration"

qError :: String -> String -> Q a
qError what context =
  fail $ "'" <> what <> "' cannot be used in a " <> context <> " context."

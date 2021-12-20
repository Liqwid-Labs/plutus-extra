{-# LANGUAGE TemplateHaskell #-}

module PlutusTx.Positive.QQ (
  positive,
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
import PlutusTx.Positive.Internal (Positive (Positive))
import Text.Read (readMaybe)
import Prelude

positive :: QuasiQuoter
positive =
  QuasiQuoter
    mkPositive
    (noPattern "positive")
    (noType "positive")
    (noDecl "positive")

mkPositive :: String -> Q Exp
mkPositive s = case readMaybe s of
  Nothing -> parseFail s
  Just (i :: Integer) -> case signum i of
    (-1) -> parseFail s
    0 -> parseFail s
    _ ->
      pure . AppE (ConE 'Positive) . LitE . IntegerL $ i

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
  fail $ "'" <> what <> "' cannot be used in a " <> context <> "context."

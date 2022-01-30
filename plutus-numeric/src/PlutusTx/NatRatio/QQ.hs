{-# LANGUAGE TemplateHaskell #-}

module PlutusTx.NatRatio.QQ (
  dec,
  frac,
) where

import Control.Arrow ((&&&))
import Data.Ratio (denominator, numerator)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Language.Haskell.TH.Syntax (
  Dec,
  Exp (AppE, ConE, LitE, UInfixE, VarE),
  Lit (IntegerL),
  Pat,
  Q,
  Type,
 )
import PlutusTx.NatRatio.Internal (NatRatio (NatRatio))
import PlutusTx.Ratio (unsafeRatio)
import Text.Read (readMaybe)
import Text.Read.Lex (Lexeme (Number), numberToRational)
import Prelude

{- | Quasi-quoter for 'NatRatio' literals, which parses a decimal
 representation.

 Used as follows:

 > [dec| 12.06 |]

 @since 1.0
-}
dec :: QuasiQuoter
dec = QuasiQuoter decExp errorDecPat errorDecType errorDecDeclaration

{- | Quasi-quoter for 'NatRatio' literals, which parses a fractional
 representation.

 Used as follows:

 > [frac| (10, 100) |]

 @since 1.0
-}
frac :: QuasiQuoter
frac = QuasiQuoter fracExp errorFracPat errorFracType errorFracDeclaration

-- Helpers

decExp :: String -> Q Exp
decExp s = case parseDecRatioExp s of
  Nothing ->
    fail $ "Input should be of the form '^[0-9]+(.[0-9]+)?(e-?[0-9]+)?$', but got: " <> s
  Just (n, m) ->
    pure $
      AppE (ConE 'NatRatio) $
        UInfixE
          (LitE $ IntegerL n)
          (VarE 'unsafeRatio)
          (LitE $ IntegerL m)

parseDecRatioExp :: String -> Maybe (Integer, Integer)
parseDecRatioExp s = readMaybe @Lexeme s >>= toRat
  where
    toRat :: Lexeme -> Maybe (Integer, Integer)
    toRat (Number n) = pure $ numerator &&& denominator $ numberToRational n
    toRat _ = Nothing

errorDecPat :: String -> Q Pat
errorDecPat _ = fail "Cannot use 'dec' in a pattern context."

errorDecType :: String -> Q Type
errorDecType _ = fail "Cannot use 'dec' in a type context."

errorDecDeclaration :: String -> Q [Dec]
errorDecDeclaration _ = fail "Cannot use 'dec' in a declaration context."

fracExp :: String -> Q Exp
fracExp s = case readMaybe @(Integer, Integer) s of
  Nothing -> fail $ "Input should be of the form (n, m), but got: " <> s
  Just (n, m) -> case signum m of
    (-1) -> fail "Cannot use a negative literal for a NatRatio denominator."
    0 -> fail "A NatRatio cannot have denominator 0."
    _ -> case signum n of
      (-1) -> fail "Cannot use a negative literal for a NatRatio numerator."
      _ ->
        pure $
          AppE (ConE 'NatRatio) $
            UInfixE
              (LitE $ IntegerL n)
              (VarE 'unsafeRatio)
              (LitE $ IntegerL m)

errorFracPat :: String -> Q Pat
errorFracPat _ = fail "Cannot use 'frac' in a pattern context."

errorFracType :: String -> Q Type
errorFracType _ = fail "Cannot use 'frac' in a type context."

errorFracDeclaration :: String -> Q [Dec]
errorFracDeclaration _ = fail "Cannot use 'frac' in a declaration context."

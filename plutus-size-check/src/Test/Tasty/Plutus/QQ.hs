{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Test.Tasty.Plutus.QQ (bytes, kbytes) where

import Language.Haskell.TH (
  Dec,
  Exp (AppE, ConE, LitE),
  Lit (IntegerL),
  Pat,
  Q,
  Type,
 )
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Test.Tasty.Plutus.Internal (ByteSize (ByteSize))
import Text.Read (readMaybe)

{- | Quasi-quoter for byte values. Will reject negative numbers, as well as
 values over 16KiB as bytes (16384), as this is the current on-chain limit.

 = Usage

 > smallSize :: ByteSize
 > smallSize = [bytes| 128 |]

 You can also use underscores to separate large literals:

 > oneKibibyte :: ByteSize
 > oneKibibyte = [bytes| 1_024 |]

 @since 1.0
-}
bytes :: QuasiQuoter
bytes = QuasiQuoter bytesExp errorPat errorType errorDeclaration

{- | Quasi-quoter for kibibytes (that is, multiples of 1024 bytes). Will reject
 negative numbers, as well as values over 16, as this is the current on-chain
 limit.

 = Usage

 > twoKibibytes :: ByteSize
 > twoKibibytes = [kbytes| 2 |]

 @since 1.0
-}
kbytes :: QuasiQuoter
kbytes = QuasiQuoter kbytesExp errorPat errorType errorDeclaration

-- Helpers

bytesExp :: String -> Q Exp
bytesExp = mkBytes "'bytes'" . readMaybe . filter (/= '_')

kbytesExp :: String -> Q Exp
kbytesExp = mkBytes "'kbytes'" . fmap (* 1024) . readMaybe

mkBytes :: String -> Maybe Int -> Q Exp
mkBytes qqName = \case
  Nothing -> fail $ qqName <> " must be given a non-negative whole number."
  Just i ->
    if
        | i < 0 ->
          fail $ qqName <> " cannot construct a negative number of bytes."
        | i > 16384 ->
          fail $
            qqName <> " cannot construct a byte value "
              <> "over the on-chain limit"
              <> " (currently 16KiB)."
        | otherwise ->
          pure
            . AppE (ConE 'ByteSize)
            . LitE
            . IntegerL
            . fromIntegral
            $ i

errorPat :: String -> Q Pat
errorPat = errorContext "pattern"

errorType :: String -> Q Type
errorType = errorContext "type"

errorDeclaration :: String -> Q [Dec]
errorDeclaration = errorContext "declaration"

errorContext :: String -> String -> Q a
errorContext con qqName =
  fail $ "Cannot use '" <> qqName <> "' in a " <> con <> "context."

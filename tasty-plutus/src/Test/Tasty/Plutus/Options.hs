module Test.Tasty.Plutus.Options (
  Fee (..),
  TimeRange (..),
  TestTxId (..),
  TestCurrencySymbol (..),
  TestValidatorHash (..),
  PropertyTestCount,
  PropertyMaxSize,
) where

import Data.Tagged (Tagged (Tagged))
import GHC.Exts (IsString)
import Ledger.Value (CurrencySymbol (CurrencySymbol), Value)
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Interval qualified as Interval
import Plutus.V1.Ledger.Scripts (ValidatorHash)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.TxId (TxId (TxId))
import Test.Tasty.Options (
  IsOption (
    defaultValue,
    optionHelp,
    optionName,
    parseValue,
    showDefaultValue
  ),
 )
import Test.Tasty.Plutus.Internal (PropertyMaxSize, PropertyTestCount)
import Prelude

{- | The transaction fee used for the tests.

 Defaults to 'mempty'. Parsing this option is currently not supported.

 @since 3.0
-}
newtype Fee = Fee Value
  deriving stock
    ( -- | @since 3.0
      Show
    )

-- | @since 3.0
instance IsOption Fee where
  defaultValue = Fee mempty
  parseValue = const Nothing
  optionName = Tagged "fee"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = Just . show

{- | Valid time range for tests.

 Defaults to 'Interval.always'. Parsing this option is currently not supported.

 @since 3.0
-}
newtype TimeRange = TimeRange (Interval POSIXTime)
  deriving stock
    ( -- | @since 3.0
      Show
    )

-- | @since 3.0
instance IsOption TimeRange where
  defaultValue = TimeRange Interval.always
  parseValue = const Nothing
  optionName = Tagged "time-range"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = Just . show

{- | The 'TxId' whose inputs should be consumed.

 The default value is arbitrary - if you need a specific value, set it
 manually. Parsing this option is currently not supported.

 @since 3.0
-}
newtype TestTxId = TestTxId TxId
  deriving stock
    ( -- | @since 3.0
      Show
    )

-- | @since 3.0
instance IsOption TestTxId where
  defaultValue = TestTxId . TxId $ "abcd"
  parseValue = const Nothing
  optionName = Tagged "tx-id"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = const Nothing

{- | The 'CurrencySymbol' used in the tests.

 The default value is arbitrary - if you need a specific value, set it
 manually. Parsing this option is currently not supported.

 @since 3.0
-}
newtype TestCurrencySymbol = TestCurrencySymbol CurrencySymbol
  deriving stock
    ( -- | @since 3.0
      Show
    )
  deriving
    ( -- | @since 3.0
      IsString
    )
    via CurrencySymbol

-- | @since 3.0
instance IsOption TestCurrencySymbol where
  defaultValue = "ff"
  parseValue = const Nothing
  optionName = Tagged "currency-symbol"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = const Nothing

{- | Validator address during testing.

 The default value is arbitrary - if you need a specific value, set it
 manually. Parsing this option is currently not supported.

 @since 3.0
-}
newtype TestValidatorHash = TestValidatorHash ValidatorHash
  deriving stock
    ( -- | @since 3.0
      Show
    )
  deriving
    ( -- | @since 3.0
      IsString
    )
    via ValidatorHash

-- | @since 3.0
instance IsOption TestValidatorHash where
  defaultValue = "90ab"
  parseValue = const Nothing
  optionName = Tagged "validator-hash"
  optionHelp = Tagged "CLI PASSING NOT SUPPORTED"
  showDefaultValue = const Nothing

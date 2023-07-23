module Utils(failure) where
import Data.Aeson (Value)
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Text.Printf (printf)



{-| a more convenient function for reporting type errors -}
failure :: String -> String -> Value -> Parser a
failure name expectedValue actualValue = 
  prependFailure
          (printf "Parsing %s failed, " name)
          (typeMismatch expectedValue actualValue)

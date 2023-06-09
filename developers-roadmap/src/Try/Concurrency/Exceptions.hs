{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Try.Concurrency.Exceptions where

import Control.Monad.Logger.CallStack
import Data.String.Interpolate
import GHC.Enum (Enum (..))
import UnliftIO (finally, timeout, tryAny)
import UnliftIO.Concurrent

oneSecond, fiveSeconds :: Int
oneSecond = 1000000
fiveSeconds = 5000000

main :: IO ()
main = runStdoutLoggingT do
  res <- timeout oneSecond $ do
    logInfo [i|Inside the timeout|]
    res <-
      tryAny $
        threadDelay fiveSeconds
          `finally` logInfo "Inside the finally"
    logInfo [i|Result: #{res}|]
  logInfo [i|After timeout: #{res}|]
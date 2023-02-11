{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Control.Monad (when)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import GHC.IO.Exception (ExitCode (..))
import System.Exit (exitFailure, exitSuccess)
import Turtle (Alternative (empty), shellStrictWithErr)

main :: IO ()
main = do
  putStrLn "Converting README"
  (exitCode, _stdout, stderr_) <-
    shellStrictWithErr
      [i|
        lima hs2md -f README.hs
        mv README.hs.md README.md
      |]
      empty
  -- Text.putStrLn stderr_
  -- Text.putStrLn stdout
  when
    (exitCode /= ExitSuccess || not (Text.null stderr_))
    (putStrLn "Failed to convert files. Exiting ..." >> exitFailure)
  exitSuccess
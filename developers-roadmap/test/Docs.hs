{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Converter
import Data.Text qualified as T
import Turtle

main :: IO ()
main = do
  putStrLn "Converting README"
  sh $ liftIO $ do
    readme <- ((T.pack . hsToMd def . T.unpack) <$>) . readTextFile $ "README.hs"
    writeTextFile "README.md" readme
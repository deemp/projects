{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq                  -- package wreq
import Control.Lens                  -- package lens
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectory)

main = do
  -- make a requeset
  r <- get "https://www.google.com/robots.txt"
  -- get the contents (as a lazy ByteString)
  let contents = r ^. responseBody
  createDirectory "./repos"
  -- write it to a local file
  BL.writeFile "./repos/myfile.txt" contents
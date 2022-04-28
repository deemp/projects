#!/usr/bin/env stack
{- stack
  script
  --resolver lts-18.28
  --install-ghc
  --package wreq
  --package lens
  --package bytestring
  --package directory
  --package uri
  --package filepath
  --package async
  --package fmt
  --package MissingH
  --package zip
  --package conduit-extra
  --package containers
  --package process
  --package aeson
  --package scientific
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Main (
  main,
)where

import           Codec.Archive.Zip        (getEntries, getEntry, getEntryName,
                                           mkEntrySelector, sourceEntry,
                                           unpackInto, withArchive)
import           Control.Applicative      (empty)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Lens
import           Control.Monad            (guard, unless)
import           Data.Aeson               (FromJSON (parseJSON), Object,
                                           Value (Number, Object), decode, (.:),
                                           (.:?))
import           Data.Aeson.Types         (Array, FromJSON (parseJSON), Parser,
                                           parseMaybe)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Conduit.Binary      as CB
import           Data.Either              (fromLeft, fromRight, isLeft, isRight)
import           Data.Foldable            (forM_)
import           Data.List.Utils          (replace)
import qualified Data.Map                 as M
import           Data.Maybe               (fromJust, fromMaybe, isJust)
import           Data.Scientific          (scientific)
import qualified Data.String              as BL
import           Fmt                      (Buildable (build), Builder,
                                           blockListF, fmt, format, (+|), (|+),
                                           (|++|))
import           Fmt.Internal.Core        (FromBuilder)
import           Network.Wreq
import           System.Directory         (createDirectory,
                                           createDirectoryIfMissing,
                                           doesDirectoryExist,
                                           getDirectoryContents)
import           System.FilePath.Posix    (takeBaseName, takeDirectory)
import           System.Process
import           Text.Printf              (printf)
import           Text.URI                 (URI (uriPath), parseURI)

main :: IO ()
main = do
  downloadAndProcessRepos _URLs _PATH
  let path = unzipped _PATH
  p <- runSloc _LANGUAGE path
  case p of
    Just (PyExtension g) -> do
      putStrLn ("Files in " +| path |+ " contain " +| g |+ " SLOC in " +\ _LANGUAGE  :: String)
    Nothing ->
      putStrLn (path |+ " doesn't contain " +| _LANGUAGE |+ " code" :: String)
  putStrLn "Done!"

_URLs :: [String]
_URLs = [
    "https://github.com/django/django/archive/refs/heads/main.zip",
    "https://github.com/matplotlib/matplotlib/archive/refs/heads/main.zip",
    "https://github.com/keras-team/keras/archive/refs/heads/master.zip",
    "https://github.com/taseikyo/PyQt5-Apps/archive/refs/heads/master.zip",
    "https://github.com/pallets/flask/archive/refs/heads/main.zip",
    "https://github.com/ansible/ansible/archive/refs/heads/devel.zip",
    "https://github.com/zulip/zulip/archive/refs/heads/main.zip",
    "https://github.com/Theano/Theano/archive/refs/heads/master.zip"
  ]

_PATH :: String
_PATH = "repos"

_CODE_JSON :: String
_CODE_JSON = "code"

_LANGUAGE :: String
_LANGUAGE = "Python"

{-

>>>zipped "path"
"path/zipped"
-}
zipped :: String -> String
zipped x = x |+ "/zipped"

{- |

>>>unzipped "path"
"path/unzipped"
-}
unzipped :: String -> String
unzipped x = x |+ "/unzipped"


data NameURL = NameURL {getName :: String, getURL :: String} deriving (Show)

{- | get name from URL

>>>getNameURL <$> _URLs
NOW [Just (NameURL {getName = "matplotlib.matplotlib.archive.refs.tags.v3.5.1.zip", getURL = "https://github.com/matplotlib/matplotlib/archive/refs/tags/v3.5.1.zip"}),Just (NameURL {getName = "django.django.archive.refs.tags.4.0.4.zip", getURL = "https://github.com/django/django/archive/refs/tags/4.0.4.zip"}),Just (NameURL {getName = "Inno-Notes.Notes.archive.refs.heads.main.zip", getURL = "https://github.com/Inno-Notes/Notes/archive/refs/heads/main.zip"})]

-}
getNameURL :: String -> Maybe NameURL
getNameURL s = p
  where
    uri = parseURI s
    name = replace "/" "." . tail . uriPath <$> uri
    url = (uriPath <$> uri) >> return s
    p = NameURL <$> name <*> url


{-
>>>checkExists (_PATH /+\ "/unzipped") []
Left "No such directory exists: repos/unzipped"
-}
checkExists :: FilePath -> b -> IO (Either String b)
checkExists p ok = do
  is <- doesDirectoryExist p
  if not is
    then do
      let msg = "No such directory exists: " +\ p :: String
      putStrLn msg
      return (Left msg)
    else return (Right ok)



{- | filter out

* URLs for already unzipped projects

* invalid URLs

>>>missing (_PATH /+\ "/unzipped") _URLs
NOW Right []
-}
missing :: FilePath -> [String] -> IO (Either String [NameURL])
missing p urls = do
  ex <- checkExists p []
  case ex of
    Left msg -> do
      putStrLn msg
      return ex
    _ -> do
      t <- getDirectoryContents p
      let nus = fromJust <$> filter isJust (getNameURL <$> urls)
          nus' = filter (\NameURL{..} -> takeBaseName getName `notElem` t) nus
      return (Right nus')

(+\) :: (FromBuilder b, Buildable a) => Builder -> a -> b
b +\ x = b +| x |+ ""

(/+\) :: (Buildable a, Buildable b, FromBuilder c) => a -> b -> c
a /+\ b = "" +| a |++| b |+ ""

{- | Given a path and a URL

downloads a file and returns its name

>>>downloadRepo (zipped "repos") (NameURL {getName = "Inno-Notes.Notes.archive.refs.heads.main.zip", getURL = "https://github.com/Inno-Notes/Notes/archive/refs/heads/main.zip"})
Right "repos/zipped/Inno-Notes.Notes.archive.refs.heads.main.zip"
-}

downloadRepo :: FilePath -> NameURL -> IO (Either String FilePath)
downloadRepo dir nu = do

  -- should I check existence here?
  exists <- doesDirectoryExist dir
  if not exists
  then return $ Left ("No such directory: " +\ dir)
  else do
    let url = getURL nu

    putStrLn ("Downloading " +\ url :: String)
    d <- get url
    let
      contents = d ^. responseBody
      name = getName nu
      path = dir |+ "/" +\ name

    BL.writeFile path contents
    putStrLn ("Downloaded " +| url |+ " into " +\ path :: String)
    return $ Right path

{- Given path to a `fileName.zip` and a `directory`,
unzip this file into `directory/fileName`

-}
unzipRepo :: FilePath -> FilePath -> IO (Either String FilePath)
unzipRepo p t = do
  let
    name = takeBaseName p
    newDir = (t |+ "/" +\ name) :: FilePath
  createDirectoryIfMissing True newDir
  putStrLn ("Unzipping " +| p |+ " into " +\ newDir :: String)
  withArchive p (unpackInto newDir)
  putStrLn ("Unzipped " +| p |+ " into " +\ newDir :: String)
  return $ Right newDir


putContent :: FilePath -> NameURL -> FilePath -> IO (Either String FilePath)
putContent zdir nu uzdir = do
  r <- downloadRepo zdir nu
  case r of
    Left msg -> do
      putStrLn msg
      return r
    Right path -> do
      k <- unzipRepo path uzdir
      case k of
        Left msg -> do
          putStrLn msg
          return r
        Right path' ->
          return k

{- |

> downloadAndProcessRepos urls dir
downloads files accessible by `urls` into directory `dir`

>>>downloadAndProcessRepos _URLs _PATH
-}
downloadAndProcessRepos :: [String] -- ^ list of possibly correct URLs
                        -> FilePath -- ^ directory to store files into
                        -> IO ()
downloadAndProcessRepos urls path = do
  let zdir = zipped path
      uzdir = unzipped path

  createDirectoryIfMissing True zdir
  createDirectoryIfMissing True uzdir

  ms <- missing uzdir urls

  case ms of
    Left msg -> do
      putStrLn msg
    Right v -> do
        unless (null v) $ putStrLn "Missing repos:"
        forM_ v (\NameURL{..} -> putStrLn getName)

  let Right ms' = ms
  zips <- mapConcurrently (\x -> putContent zdir x uzdir) ms'
  return ()

type Language = String

{-
>>>runSloc _LANGUAGE "app"
Just (HsExtension 164)
-}

runSloc :: Language -> FilePath -> IO (Maybe PyExtension)
runSloc lang path = do
  p <- readProcess "sloc" ["-f", "json", path] ""
  return (decode (BL.fromString p) :: Maybe PyExtension)

newtype PyExtension = PyExtension (Maybe Int) deriving Show

instance FromJSON PyExtension where
  parseJSON (Object v) = do
    ext <- v .: "byExt"
    hs <- ext .:? "py"
    cnt <-
      case hs of
        Just k -> do
          k1 <- k .: "summary"
          k1 .: "source"
        Nothing -> return Nothing
    return (PyExtension cnt)
  parseJSON _ = empty

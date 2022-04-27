{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main (
  main,
)where

import Network.Wreq                  -- package wreq
import Control.Lens                  -- package lens
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectory, getDirectoryContents, doesDirectoryExist, createDirectoryIfMissing)
import Text.Printf (printf)
import Text.URI ( parseURI, URI(uriPath) )
import System.FilePath.Posix(takeBaseName, takeDirectory)
import Control.Concurrent.Async (mapConcurrently)
import Fmt ( (+|), (|+), format, Buildable(build), Builder, (|++|), blockListF, fmt )
import Fmt.Internal.Core (FromBuilder)
import Data.List.Utils ( replace )
import Control.Monad (guard)
import Data.Maybe (isJust, fromJust)
import Data.Either (isRight, isLeft, fromRight, fromLeft)
import Codec.Archive.Zip (withArchive, sourceEntry, mkEntrySelector, getEntries, getEntry, getEntryName)
import qualified Data.Conduit.Binary as CB
import qualified Data.Map as M

main :: IO ()
main = do
  downloadAndProcessRepos _URLs "repos"

_URLs :: [String]
_URLs = [
    -- "https://github.com/matplotlib/matplotlib/archive/refs/tags/v3.5.1.zip",
    "https://github.com/br4ch1st0chr0n3/pyrepos/archive/refs/heads/master.zip",
    "https://github.com/Inno-Notes/Notes/archive/refs/heads/main.zip"
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


{- | filter out 

* URLs for already unzipped projects

* invalid URLs

>>>missing _PATH _URLs
NOW Right [NameURL {getName = "matplotlib.matplotlib.archive.refs.tags.v3.5.1.zip", getURL = "https://github.com/matplotlib/matplotlib/archive/refs/tags/v3.5.1.zip"},NameURL {getName = "django.django.archive.refs.tags.4.0.4.zip", getURL = "https://github.com/django/django/archive/refs/tags/4.0.4.zip"},NameURL {getName = "Inno-Notes.Notes.archive.refs.heads.main.zip", getURL = "https://github.com/Inno-Notes/Notes/archive/refs/heads/main.zip"}]
-}
missing :: FilePath -> [String] -> IO (Either String [NameURL])
missing p urls = do
  guard =<< doesDirectoryExist p
  t <- getDirectoryContents p
  let nus = fromJust <$> filter isJust (getNameURL <$> urls)
  return (Right nus)

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

    print ("Downloading " +\ url :: String)
    d <- get url
    let
      contents = d ^. responseBody
      name = getName nu
      path = dir |+ "/" +\ name

    BL.writeFile path contents
    print ("Downloaded " +| url |+ " into " +\ path :: String)
    return $ Right path

{-
>>>takeBaseName "repos/unzipped/Inno-Notes.Notes.archive.refs.heads.main.zip"
"Inno-Notes.Notes.archive.refs.heads.main"
-}

{- Given path to a `fileName.zip` and a `directory`, 
unzip this file into `directory/fileName`

-}
unzipRepo :: FilePath -> FilePath -> IO ()
unzipRepo p t = do
  let
    name = takeBaseName p
    newDir = (t |+ "/" +\ name) :: FilePath
    -- newPath = (newDir |+ "/" +\ name) :: FilePath
  createDirectoryIfMissing True newDir
  print ("Unzipping " +| p |+ " into " +\ newDir :: String)
  entries <- withArchive p (M.keys <$> getEntries)
  case entries of
    [] -> "The archive " +| p |+ "is empty"
    _ ->
      do
        print (getEntryName <$> entries)
        let dirs = (\x -> newDir |+ "/" +\ getEntryName x) <$> entries
        mapM_ (createDirectoryIfMissing True . takeDirectory) dirs
        let es = zip entries (CB.sinkFile <$> dirs)
        bs <- mapM (\(x, e) -> withArchive p $ sourceEntry x e) es
        print ("Unzipped " +| p |+ " into " +\ newDir :: String)

{- | Given a list of URLs and a target directory,

download files there

>>>downloadRepos _URLs _PATH
-}
downloadAndProcessRepos :: [String] -> FilePath -> IO ()
downloadAndProcessRepos urls path = do
  -- TODO check doesn't overwrite if exist
  let zdir = zipped path
  createDirectoryIfMissing True zdir
  ms <- missing zdir urls
  case ms of
    Left msg -> do
      print msg
      return ()
    _ -> pure ()

  let Right ms' = ms
  zips <- mapConcurrently (downloadRepo zdir) ms'
  let
    zipl = filter isLeft zips & map (fromLeft "")
    zipr = filter isRight zips & map (fromRight "")

  let uzdir = unzipped path
  createDirectoryIfMissing True uzdir

  unzips <- mapConcurrently (`unzipRepo` uzdir) zipr
  return ()

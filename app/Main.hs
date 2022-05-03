
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
  --package text
  --package rio
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
)where

import           Codec.Archive.Zip        (getEntries, getEntry, getEntryName,
                                           mkEntrySelector, sourceEntry,
                                           unpackInto, withArchive)
import           Control.Applicative      (empty)
import           Control.Concurrent.Async (mapConcurrently)
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
import           Data.Text                (pack)
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
import           Text.URI                 (URI (uriPath, URI), parseURI)

import RIO
import RIO.List.Partial (tail)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Cont (ContT(runContT, ContT), MonadCont (callCC), Cont)
import Prelude (print, putStrLn)
import Control.Monad.Extra (doCallCC)


main :: IO ()
main = do undefined
  -- downloadAndProcessRepos _URLs _PATH
  -- let path = unzipped _PATH
  -- p <- runSloc _LANGUAGE path
  -- case p of
  --   Just (PyExtension g) -> do
  --     let msg = ("Files in " +| path |+ " contain " +| g |+ " SLOC in " +\ _LANGUAGE  :: String)
  --     putStrLn msg
  --     writeFile "report" msg
  --   Nothing ->
  --     putStrLn (path |+ " doesn't contain " +| _LANGUAGE |+ " code" :: String)
  -- putStrLn "Done!"

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

_EXTENSION :: String
_EXTENSION = "py"

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

class Convertible a where
  getName :: a -> String
  getPath :: a -> String
  getURI :: a -> String

instance Convertible URI where
  getName URI {..} = uriPath & tail & replace "/" "."
  getPath URI {..} = uriPath
  getURI = show



-- data NameURL = NameURL {getName :: String, getURL :: String} deriving (Show)

{- | get name from URL

>>>getNameURL <$> _URLs
NOW [Just (NameURL {getName = "matplotlib.matplotlib.archive.refs.tags.v3.5.1.zip", getURL = "https://github.com/matplotlib/matplotlib/archive/refs/tags/v3.5.1.zip"}),Just (NameURL {getName = "django.django.archive.refs.tags.4.0.4.zip", getURL = "https://github.com/django/django/archive/refs/tags/4.0.4.zip"}),Just (NameURL {getName = "Inno-Notes.Notes.archive.refs.heads.main.zip", getURL = "https://github.com/Inno-Notes/Notes/archive/refs/heads/main.zip"})]

-}
-- getNameURL :: String -> Maybe URI
-- getNameURL s = 
--   where
--     uri = parseURI s
--     name = replace "/" "." . tail . uriPath <$> uri
--     url = uriPath <$> uri >> return s
--     p = NameURL <$> name <*> url


{-
>>>checkExists (_PATH /+\ "/unzipped") []
Left "No such directory exists: repos/unzipped"
-}

data LocationError =
    NoSuchDirectory FilePath
  | NoSuchFile FilePath
  | ProblematicURL URI
  deriving Show

instance Display LocationError where
  display (NoSuchDirectory s) = RIO.displayBytesUtf8 ("No such directory exists: " +\ s)
  display (NoSuchFile s) = RIO.displayBytesUtf8 ("No such file exists: " +\ s)
  display (ProblematicURL s) = RIO.displayBytesUtf8 ("Unavailable URL: " +\ show s)


checkExists :: FilePath -> IO Bool
checkExists p = runSimpleApp $ do
  is <- liftIO $ doesDirectoryExist p
  unless is (logInfo $ display (NoSuchDirectory p))
  return is

{- | filter out

* URLs for already unzipped projects

* invalid URLs

>>>missing (_PATH /+\ "/unzipped") (fromJust . parseURI <$> _URLs)
-}
missing :: FilePath -> [URI] -> IO (Either LocationError [URI])
missing p urls = doCallCC $ \cont -> do
  ex <- lift $ checkExists p
  unless ex $ cont (Left $ NoSuchDirectory p)
  t <- lift $ getDirectoryContents p
  let nus = filter (\x -> getName x `notElem` t) urls
  return $ Right nus

(+\) :: (FromBuilder b, Buildable a) => Fmt.Builder -> a -> b
b +\ x = b +| x |+ ""

(/+\) :: (Buildable a, Buildable b, FromBuilder c) => a -> b -> c
a /+\ b = "" +| a |++| b |+ ""

{- | Given a path and a URL

downloads a file and returns its name

>>>downloadRepo (zipped "repos") (NameURL {getName = "Inno-Notes.Notes.archive.refs.heads.main.zip", getURL = "https://github.com/Inno-Notes/Notes/archive/refs/heads/main.zip"})
Right "repos/zipped/Inno-Notes.Notes.archive.refs.heads.main.zip"
-}

downloadRepo :: FilePath -> URI -> IO (Either LocationError FilePath)
downloadRepo dir nu = doCallCC $ \cont -> do
  ex <- lift $ checkExists dir
  unless ex $ cont (Left $ NoSuchDirectory dir)
  let url = getURI nu
  lift $ putStrLn ("Downloading " +\ url)
  -- TODO handle invalid codes
  d <- lift $ get url
  let
    contents = d ^. responseBody
    path = dir |+ "/" +\ getName nu
  lift $ BL.writeFile path contents
  lift $ putStrLn ("Downloaded " +| url |+ " into " +\ path)
  return $ Right path

{- Given path to a `fileName.zip` and a `directory`,

unzip this file into `directory/fileName`

and return this new path
-}
unzipRepo :: FilePath -> FilePath -> IO (Either LocationError FilePath)
unzipRepo p t = doCallCC $ \cont -> do
  ex1 <- lift $ checkExists p
  unless ex1 $ cont (Left $ NoSuchDirectory p)
  ex2 <- lift $ checkExists t
  unless ex2 $ cont (Left $ NoSuchDirectory t)
  let
    name = takeBaseName p
    newDir = t |+ "/" +\ name
  lift $ createDirectoryIfMissing True newDir
  lift $ putStrLn ("Unzipping " +| p |+ " into " +\ newDir :: String)
  withArchive p (unpackInto newDir)
  lift $ putStrLn ("Unzipped " +| p |+ " into " +\ newDir :: String)
  return $ Right newDir


putContent :: FilePath -> URI -> FilePath -> IO (Either LocationError FilePath)
putContent zipdir url unzipdir = doCallCC $ \cont -> do
  -- TODO do I need these checks?
  ex1 <- lift $ checkExists zipdir
  unless ex1 $ cont (Left $ NoSuchDirectory zipdir)
  ex2 <- lift $ checkExists unzipdir
  unless ex2 $ cont (Left $ NoSuchDirectory unzipdir)
  r <- lift $ downloadRepo zipdir url
  case r of
    Left _ -> return r
    Right path -> lift $ unzipRepo path unzipdir

{- |

> downloadAndProcessRepos urls dir
downloads files accessible by `urls` into directory `dir`

>>>downloadAndProcessRepos _URLs _PATH
-}
-- downloadAndProcessRepos :: [String] -- ^ list of possibly correct URLs
--                         -> FilePath -- ^ directory to store files into
--                         -> IO ()
-- downloadAndProcessRepos urls path = do
--   let zdir = zipped path
--       uzdir = unzipped path
--   createDirectoryIfMissing True zdir
--   createDirectoryIfMissing True uzdir
--   ms <- missing uzdir urls

--   case ms of
--     Left msg -> do
--       putStrLn msg
--     Right v -> do
--         unless (null v) $ putStrLn "Missing repos:"
--         forM_ v (\NameURL{..} -> putStrLn getName)

--   let Right ms' = ms
--   zips <- mapConcurrently (\x -> putContent zdir x uzdir) ms'
--   return ()

-- type Language = String

-- {-
-- >>>runSloc _LANGUAGE "app"
-- Just (HsExtension 164)
-- -}

-- runSloc :: Language -> FilePath -> IO (Maybe PyExtension)
-- runSloc lang path = do
--   p <- readProcess "sloc" ["-f", "json", path] ""
--   return (decode (BL.fromString p) :: Maybe PyExtension)

-- newtype PyExtension = PyExtension (Maybe Int) deriving Show

-- instance FromJSON PyExtension where
--   parseJSON (Object v) = do
--     ext <- v .: "byExt"
--     hs <- ext .:? pack _EXTENSION
--     cnt <-
--       case hs of
--         Just k -> do
--           k1 <- k .: "summary"
--           k1 .: "source"
--         Nothing -> return Nothing
--     return (PyExtension cnt)
--   parseJSON _ = empty

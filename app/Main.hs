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
  --package text
  --package rio
  --package mtl
  --package monad-extras
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import           System.Directory         (createDirectoryIfMissing,
                                           doesDirectoryExist, doesFileExist,
                                           doesPathExist, getDirectoryContents)
import           System.FilePath.Posix    (joinPath, takeBaseName,
                                           takeDirectory)
import           System.Process
import           Text.Printf              (printf)
import           Text.URI                 (URI (URI, uriPath), parseURI)

import           Control.Monad.Cont       (Cont, ContT (ContT, runContT),
                                           MonadCont (callCC))
import           Control.Monad.Except     (ExceptT (ExceptT), runExceptT)
import           Control.Monad.Extra      (doCallCC)
import           Data.List                (intercalate, partition)
import           Data.Text.IO             as TIO (writeFile)
import           Prelude                  (print, putStrLn, writeFile)
import           RIO                      hiding (mapConcurrently)
import           RIO.List.Partial         (tail)

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

-- _LANGUAGE :: HaskellExt
-- _LANGUAGE = HaskellExt Nothing

_LANGUAGE :: PythonExt
_LANGUAGE = PythonExt Nothing

_CODE_JSON :: String
_CODE_JSON = "code"

_ZIPPED :: String
_ZIPPED = "zipped"

_UNZIPPED :: String
_UNZIPPED = "unzipped"


main :: IO ()
main = runSimpleApp $ doCallCC $ \cont -> do
  path <- liftIO $ createDirectoryIfMissing True _PATH
  logInfo (Utf8Builder $ "The files will be stored in \"" +| _PATH |+ "\"")
  path' <- liftIO $ downloadAndProcessRepos _URLs _PATH
  let path = unzipped _PATH
  p <- liftIO $ runSloc _LANGUAGE path
  case p of
    Right r -> do
      let msg = Utf8Builder $ "Files in " +| path |+ " contain " +| maybe "no" show (getData r) |+ " SLOC in " +\ showName r
      logInfo msg
      liftIO $ TIO.writeFile "report" (utf8BuilderToText msg)
    Left r ->
      logError (Utf8Builder $ path |+ " doesn't contain " +| showName _LANGUAGE |+ " code")
  logInfo "Done!"

{-
>>>zipped "path"
"path/zipped"
-}
zipped :: String -> String
zipped x = joinPath [x, _ZIPPED]

{- |
>>>unzipped "path"
"path/unzipped"
-}
unzipped :: String -> String
unzipped x = joinPath [x, _UNZIPPED]

class Convertible a where
  getZipName :: a -> String
  getName :: a -> String
  getPath :: a -> String
  getURI :: a -> String

{-
>>>getName $ fromJust $ parseURI "https://github.com/django/django/archive/refs/heads/main.zip"
"django.django.archive.refs.heads.main"
-}

instance Convertible URI where
  getZipName URI {..} = uriPath & tail & replace "/" "."
  getName = takeBaseName . getZipName
  getPath URI {..} = uriPath
  getURI = show

data LocationError =
    NoSuchDirectory FilePath
  | NoSuchFile FilePath
  | ProblematicURL URI
  deriving Show

instance Display LocationError where
  display (NoSuchDirectory s) = Utf8Builder ("No such directory exists: " +\ s)
  display (NoSuchFile s) = Utf8Builder ("No such file exists: " +\ s)
  display (ProblematicURL s) = Utf8Builder ("Unavailable URL: " +\ show s)


checkExists :: FilePath -> LocationError -> IO Bool
checkExists p locErr = runSimpleApp $ do
  exists <- liftIO $ doesPathExist p
  unless exists (logInfo $ display locErr)
  return exists

{- | filter out URIs for already unzipped projects

>>>getMissing (_PATH /+\ "/unzipped") (fromJust <$> (filter (isJust) (parseURI <$> _URLs)))
-}
getMissing :: FilePath -> [URI] -> IO (Either LocationError [URI])
getMissing path urls = doCallCC $ \cont -> do
  unlessValid path cont Left (NoSuchDirectory path)
  t <- lift $ getDirectoryContents path
  let nus = filter (\x -> getName x `notElem` t) urls
  return $ Right nus

(+\) :: (FromBuilder b, Buildable a) => Fmt.Builder -> a -> b
b +\ x = b +| x |+ ""

(/+\) :: (Buildable a, Buildable b, FromBuilder c) => a -> b -> c
a /+\ b = "" +| a |++| b |+ ""

{- | Given a `directory` and a `URL`

gets a `file` from `URL`

writes this `file` into `directory`

returns this `file`'s path
-}

downloadRepo :: FilePath -> URI -> IO (Either LocationError FilePath)
downloadRepo dir uri = doCallCC $ \cont -> do
  unlessValid dir cont Left (NoSuchDirectory dir)
  let url = getURI uri
  lift $ putStrLn ("Downloading " +\ url)
  -- TODO handle invalid codes
  resp <- lift $ get url
  let
    contents = resp ^. responseBody
    zipPath = dir |+ "/" +\ getZipName uri
  lift $ BL.writeFile zipPath contents
  lift $ putStrLn ("Downloaded " +| url |+ " into " +\ zipPath)
  return $ Right zipPath

{-
Given path to a `fileName.zip` and a `directory`,

unzip this file into `directory/fileName`

and return this new path
-}

-- unlessValid :: (Monad (t1 IO), MonadTrans t1) => FilePath -> (t2 -> t1 IO ()) -> t2 -> t1 IO ()
unlessValid path c ret err = do
  ex <- lift $ checkExists path err
  unless ex $ c (ret err)

unzipRepo :: FilePath -> FilePath -> IO (Either LocationError FilePath)
unzipRepo zipPath unzipDir = doCallCC $ \cont -> do
  unlessValid zipPath cont Left (NoSuchFile zipPath)
  unlessValid unzipDir cont Left (NoSuchDirectory unzipDir)
  let
    name = takeBaseName zipPath
    newDir = unzipDir |+ "/" +\ name
  lift $ createDirectoryIfMissing True newDir
  lift $ putStrLn ("Unzipping " +| zipPath |+ " into " +\ newDir)
  withArchive zipPath (unpackInto newDir)
  lift $ putStrLn ("Unzipped " +| zipPath |+ " into " +\ newDir)
  return $ Right newDir


putContent :: FilePath -> URI -> FilePath -> IO (Either LocationError FilePath)
putContent zipDir url unzipDir = doCallCC $ \cont -> do
  unlessValid zipDir cont Left (NoSuchDirectory zipDir)
  unlessValid unzipDir cont Left (NoSuchDirectory unzipDir)
  r <- lift $ downloadRepo zipDir url
  case r of
    Left _     -> return r
    Right path -> lift $ unzipRepo path unzipDir


{- |

> downloadAndProcessRepos urls dir
downloads files accessible by `urls` into directory `dir`

returns dir
>>>downloadAndProcessRepos _URLs _PATH
-}
downloadAndProcessRepos :: [String] -- ^ list of possibly correct URLs
                        -> FilePath -- ^ directory to store files into
                        -> IO (Either LocationError FilePath)
downloadAndProcessRepos urls path = runSimpleApp $ doCallCC $ \cont -> do
  let zdir = zipped path
      uzdir = unzipped path
  liftIO $ createDirectoryIfMissing True zdir
  liftIO $ createDirectoryIfMissing True uzdir
  let
    urlsGood = fromJust <$> filter isJust (parseURI <$> urls)
    urlsBad = filter (isNothing . parseURI) urls
  unless
    (null urlsBad)
    (logInfo $ Utf8Builder $ "The following URLs are unavailable:\n" +\ intercalate "\n" urlsBad)
  ms <- liftIO $ getMissing uzdir urlsGood
  when (isLeft ms) (let Left ms' = ms in cont (Left ms'))
  let Right ms' = ms
  unless
    (null ms')
    (logInfo $ Utf8Builder $ "Missing repos:\n" +\ intercalate "\n" (show <$> urlsGood))
  zips <- liftIO $ mapConcurrently (\x -> putContent zdir x uzdir) ms'
  return $ Right uzdir


newtype LanguageError a = NoLanguage {b :: (LangExtension a) => a}

instance (LangExtension a) => Show (LanguageError a) where
  show (NoLanguage m) = "No " +| showName m |+ " files detected"

{-
counts SLOC in a directory

returns Left if no info about a language is available
>>>runSloc (PythonExt Nothing) "app"
-}
runSloc :: (LangExtension a, FromJSON a) => a -> FilePath -> IO (Either (LanguageError a) a)
runSloc lang path = runSimpleApp $ doCallCC $ \cont -> do
  logInfo (Utf8Builder $ "Counting lines for " +| showName lang |+ " in " +| path |+ "...")
  p <- liftIO $ readProcess "sloc" ["-f", "json", path] ""
  let p' = decode (BL.fromString p)
  when (isNothing p') (cont $ Left $ NoLanguage lang)
  return (Right $ fromJust p')

newtype PythonExt = PythonExt (Maybe Int)
newtype JavaExt = JavaExt (Maybe Int)
newtype HaskellExt = HaskellExt (Maybe Int)

class LangExtension a where
  -- TODO allow several extensions
  showExt :: a -> String
  showName :: a -> String
  showFull :: a -> String
  getData :: a -> Maybe Int
  getNoLangError :: a -> LanguageError a

showData' :: (LangExtension a) => a -> String
showData' a = showName a |+ ": " +| maybe "?" show (getData a) |+ " SLOC"

instance LangExtension PythonExt where
  showExt _ = "py"
  showName _ = "Python"
  getData (PythonExt a) = a
  showFull = showData'
  getNoLangError = NoLanguage

instance LangExtension JavaExt where
  showExt _ = "java"
  showName _ = "Java"
  getData (JavaExt a) = a
  showFull = showData'
  getNoLangError = NoLanguage

instance LangExtension HaskellExt where
  showExt _ = "hs"
  showName _ = "Haskell"
  getData (HaskellExt a) = a
  showFull = showData'
  getNoLangError = NoLanguage

instance FromJSON PythonExt where
  parseJSON v = parseJSONExt v PythonExt

instance FromJSON JavaExt where
  parseJSON v = parseJSONExt v JavaExt

instance FromJSON HaskellExt where
  parseJSON v = parseJSONExt v HaskellExt

parseJSONExt :: (LangExtension a) => Value -> (Maybe Int -> a) -> Parser a
parseJSONExt (Object v) a = do
  ext <- v .: "byExt"
  hs <- ext .:? pack (showExt (a Nothing))
  cnt <-
    case hs of
      Just k -> do
        k1 <- k .: "summary"
        k1 .: "source"
      Nothing -> return Nothing
  return (a cnt)
parseJSONExt _ _ = undefined

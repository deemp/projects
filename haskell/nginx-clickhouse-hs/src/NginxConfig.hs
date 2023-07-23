{-# LANGUAGE OverloadedStrings #-}

module NginxConfig(NginxConfig(..), Clickhouse(..), CHColumn(..), Nginx(..)) where

import Data.Aeson.Types (prependFailure, typeMismatch, Value (String), Parser, Object, object)
import Data.Yaml
  ( FromJSON (parseJSON),
    ParseException,
    Value (Object),
    decodeFileEither,
    (.:),
  )
import Data.Aeson.KeyMap (toList, KeyMap, toAscList)
import Data.Aeson.Key (toString)
import Data.Text (unpack)
import Text.Printf
import Utils(failure)
import Control.Monad (forM)

data Settings = Settings
  { interval :: Int,
    logPath :: FilePath,
    seekFromEnd :: Bool
  }
  deriving (Show)

instance FromJSON Settings where
  parseJSON (Object v) =
    Settings
      <$> v .: "interval"
      <*> v .: "log_path"
      <*> v .: "seek_from_end"
  parseJSON invalid = failure "Settings" "Object" invalid


data Credentials = Credentials
  { user :: String,
    password :: String
  }
  deriving (Show)

instance FromJSON Credentials where
  parseJSON (Object v) =
    Credentials
      <$> v .: "user"
      <*> v .: "password"
  parseJSON invalid = failure "Credentials" "Object" invalid

data Clickhouse = Clickhouse
  { db :: String,
    table :: String,
    host :: String,
    port :: Int,
    credentials :: Credentials,
    columns :: [CHColumn]
  }
  deriving (Show)

instance FromJSON Clickhouse where
  parseJSON (Object v) =
    -- TODO check all columns are of required form
    Clickhouse
      <$> v .: "db"
      <*> v .: "table"
      <*> v .: "host"
      <*> v .: "port"
      <*> v .: "credentials"
      <*> cs
      where
        cs = do
          p <- toAscList <$> (v .: "columns" :: Parser Object)
          let p' = object . (:[]) <$> p
          traverse (\x -> parseJSON x :: Parser CHColumn) p'
  parseJSON invalid = failure "ClickHouse" "Object" invalid

data CHColumn = CHColumn {
    name :: String,
    magicWord :: String
  } deriving Show

instance FromJSON CHColumn where
  parseJSON val@(Object v) =
    case t of
      Just t' -> return t'
      _ -> failure "CHColumn" "Object" val
    where
      (k, val) = (head . toList) v
      t =
        case val of
          String s -> Just $ CHColumn (toString k) (unpack s)
          _ -> Nothing
  parseJSON invalid = failure "CHColumn" "Object" invalid


data Nginx = Nginx
  { logType :: String,
    logFormat :: String
  }
  deriving (Show)

instance FromJSON Nginx where
  parseJSON (Object v) =
    Nginx
      <$> v .: "log_type"
      <*> v .: "log_format"
  parseJSON invalid = failure "Nginx" "Object" invalid


data NginxConfig = NginxConfig
  { settings :: Settings,
    clickhouse :: Clickhouse,
    nginx :: Nginx
  }
  deriving (Show)

instance FromJSON NginxConfig where
  parseJSON (Object v) =
    NginxConfig
      <$> v .: "settings"
      <*> v .: "clickhouse"
      <*> v .: "nginx"
  parseJSON invalid = failure "NginxConfig" "Object" invalid


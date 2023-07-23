{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module SqlConfig(SQLExpression(..), DBType(..), Create (..), DBColumnDescription(..)) where


import Data.Aeson.Key (toString)
import Data.Aeson.KeyMap (toList, toAscList)
import Data.Aeson.Types (Object, Parser, Value (String), prependFailure, typeMismatch, (.:?))
import Data.Functor ((<&>))
import Data.Yaml
  ( FromJSON (parseJSON),
    ParseException,
    Value (Object),
    decodeFileEither,
    (.:),
  )
import Lib
import NginxConfig (NginxConfig (..))
import System.Directory
import Text.Pretty.Simple
import Text.Printf
import qualified Data.Maybe
import Utils(failure)

data DBType
  = DBString
  | DBDateTime
  | DBFloat32
  | DBFixedString Int
  | DBInt32
  | DBInt64
  | Date
  deriving (Show)

instance FromJSON DBType where
  parseJSON val@(String s) =
    case getType s of
      Just v -> return v
      Nothing -> failure "DBType" "String" val
    where
      getType s
        | s == "String" = Just DBString
        | s == "DateTime" = Just DBDateTime
        | s == "Float32" = Just DBFloat32
        | s == "Int32" = Just DBInt32
        | s == "Int64" = Just DBInt64
        | s == "FixedString(2)" = Just (DBFixedString 2)
        | s == "Date" = Just Date
        | otherwise = Nothing
  parseJSON invalid = failure "DBType" "String" invalid
    
data DBColumnDescription = DBColumnDescription
  { name :: String,
    entryType :: DBType,
    defaultValue :: Maybe String
  }
  deriving (Show)


data DBEngine = MergeTree deriving (Show)

data Create = Create
  { table :: String,
    columns :: [DBColumnDescription],
    engine :: DBEngine,
    partitionBy :: String,
    orderBy :: String,
    settings :: String
  }
  deriving (Show)

instance FromJSON DBEngine where
  parseJSON val@(String s)
    | s == "MergeTree" = return MergeTree
    | otherwise = failure "DBEngine" "String" val
  parseJSON invalid = failure "DBType" "String" invalid

instance FromJSON DBColumnDescription where
  parseJSON val@(Object v) = 
    case desc of 
      Just v -> v
      Nothing -> failure "DBColumnDescription" "Object" val
    where
      (k, val) = (head . toAscList) v
      db e d =  DBColumnDescription {name = toString k, entryType = e, defaultValue = d}
      desc =
        case val of
          String s ->
            Just $ (\x -> db x Nothing) <$> (parseJSON val :: Parser DBType)
          Object v' -> Just $ db <$> (v' .: "TYPE") <*> v' .:? "DEFAULT"
          _ -> Nothing
  parseJSON invalid = failure "DBColumnDescription" "Object" invalid

instance FromJSON Create where
  parseJSON (Object v) =
    Create
      <$> v .: "TABLE"
      <*> v .: "COLUMNS"
      <*> v .: "ENGINE"
      <*> v .: "PARTITION BY"
      <*> v .: "ORDER BY"
      <*> v .: "SETTINGS"
  parseJSON invalid = failure "Create" "Object" invalid

data SQLExpression = CREATE Create deriving Show

instance FromJSON SQLExpression where
    parseJSON (Object v) = CREATE <$> v .: "CREATE"
    parseJSON invalid = failure "Expression" "Object" invalid

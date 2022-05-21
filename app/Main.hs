{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict as HashMap (HashMap, fromList, lookup)
import Data.Maybe (fromJust)
import qualified Data.Maybe
import Data.Yaml
  ( FromJSON (parseJSON),
    ParseException,
    Value (Object),
    decodeFileEither,
    (.:),
  )
import Lib
import NginxConfig (CHColumn (..), Clickhouse (..), NginxConfig (..), Nginx(..))
import SqlConfig (Create (..), DBColumnDescription (..), DBType (..), SQLExpression (..))
import System.Directory
import Text.Pretty.Simple
import Text.Printf
import Text.Megaparsec as MP
import Data.Time (UTCTime, CalendarDiffDays)
import Data.Void
import Data.Text (Text, unpack, pack)
import Text.Megaparsec.Char (string, letterChar, numberChar, char, printChar, spaceChar)
import Data.List (partition)
import Data.Traversable (for, forM)
import Text.Megaparsec.Debug (dbg)

nginxConfigPath :: FilePath
nginxConfigPath = "./files/nginx_config.yml"

sqlConfigPath :: FilePath
sqlConfigPath = "./files/schema.yml"

type MagicWord = String

data ColumnMatch = ColumnMatch
  { column :: String,
    word :: MagicWord,
    dbType :: DBType
  }
  deriving (Show)

matchDBTypesFromSQL :: NginxConfig.NginxConfig -> SQLExpression -> [ColumnMatch]
matchDBTypesFromSQL ng (CREATE c) = matches
  where
    sqlColumns = c & SqlConfig.columns <&> (\(DBColumnDescription n t d) -> (n, t)) & fromList
    -- TODO handle no DBType cases
    matches =
      ng & clickhouse & NginxConfig.columns
        <&> ( \(CHColumn n m) ->
                ColumnMatch n m (fromJust $ HashMap.lookup n sqlColumns)
            )

data DBEntry =
    DBString String
  | DBDateTime UTCTime
  | DBFloat32 Float
  | DBFixedString Int String
  | DBInt32 Int
  | DBInt64 Int
  | Date CalendarDiffDays
  deriving (Show)



type MParser = Parsec Void String

data FormatToken = TWord {lParen::String, magicWord :: String, rParen::String} | TDelim String deriving Show

pMagicWord :: MParser String
pMagicWord = do
  _ <- string "$"
  some (letterChar <|> numberChar <|> char '_')

lparens = ['(', '[', '"', '\'']
rparens = [')', ']', '"', '\'']
delims = ['-']


pFormatToken :: MParser FormatToken
pFormatToken = do
  choice [
    do
      l <- many $ oneOf lparens
      w <- pMagicWord
      r <- many $ oneOf rparens
      return $ TWord l w (r <> " ")
    , do
        d <- some $ oneOf delims
        return (TDelim d)
    ]

{-
>>>parseMaybe pFormat "$remote_addr - $remote_user [$time_local] \"$request\""
Just [TWord "remote_addr",TBetween " - ",TWord "remote_user",TBetween " [",TWord "time_local",TBetween "] \"",TWord "request",TBetween "\""]
-}

tokenizeFormat :: NginxConfig -> Maybe [FormatToken]
tokenizeFormat ng =
  do
    let ws = words (ng & nginx & logFormat)
    traverse (parseMaybe pFormatToken) ws

logLine :: String
logLine = "157.90.181.51 - - [06/Apr/2022:00:17:01 +0300] 'GET /version HTTP/1.1' 'GET' '-' '0.005' 200 341 '-' 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/38.0.2125.104 Safari/537.36' '0.005' 'north-west.rt.getshop.tv' 'spb-1' '-' '-' '-' '-' '{\x22versions\x22:[\x221.8.0\x22],\x22server_version\x22:\x221.25.1\x22,\x22build\x22:null,\x22git\x22:{\x22commit\x22:null,\x22branch\x22:null,\x22tag\x22:null},\x22docker\x22:null}' '-' '-' '-' '-' '-' '-' '-' '-' '-' '-' '-' '-'"

-- pAnyUntil :: String -> MParser String
-- pAnyUntil s = some (printChar <* lookAhead (string s))

pBetween :: String -> String -> String -> MParser String
pBetween lp w rp = do
  _ <- dbg "left paren" $ string lp
  s <- dbg "words" $ takeWhileP Nothing (/= head rp)
  _ <- dbg "right paren" $ string rp
  return s

pDelim :: String -> MParser String
pDelim s = do
  s' <- dbg "delim" $ string (s ++ " ")
  return s'

tokenizeLine :: [FormatToken] -> MParser [(MagicWord, String)]
tokenizeLine fs = do
  p <- forM fs (\case TWord lp w rp -> pBetween lp w rp; TDelim s -> pDelim s)
  let ws = (\case TWord _ w _ -> w; TDelim w -> w) <$> fs
  return $ zip ws p

{-
>>>parseMaybe (pUntilBtwAndConsume "[") "skdlfk4"
Nothing
-}
-- pDBString :: MParser DBEntry
-- pDBString = 

-- getMagicParsers :: HashMap DBType DBEntry
-- getMagicParsers = undefined



main :: IO ()
main = do

  (nginxConfig :: Either ParseException NginxConfig.NginxConfig) <- decodeFileEither nginxConfigPath
  (sqlConfig :: Either ParseException [SQLExpression]) <- decodeFileEither sqlConfigPath

  let p = matchDBTypesFromSQL <$> nginxConfig <*> (head <$> sqlConfig)
  let f = tokenizeFormat <$> nginxConfig
  pPrint f
  case f of
    Right (Just f') -> do
      print logLine
      parseTest (tokenizeLine f') (logLine <> " ")
    _ -> print "oops"
  -- print $ ((`parseMaybe` logLine) . tokenizeLine <$>) <$> f
  -- pPrint nginxConfig
  -- pPrint sqlConfig
  return ()

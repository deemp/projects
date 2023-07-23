{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashMap.Strict as HashMap (HashMap, fromList, lookup)
import Data.List (partition)
import Data.Maybe (fromJust)
import qualified Data.Maybe
import Data.Text (Text, pack, unpack)
import Data.Time (CalendarDiffDays, UTCTime, defaultTimeLocale, parseTimeM)
import Data.Traversable (for, forM)
import Data.Void
import Data.Yaml
  ( FromJSON (parseJSON),
    ParseException,
    Value (Object),
    decodeFileEither,
    (.:),
  )
import Lib
import NginxConfig (CHColumn (..), Clickhouse (..), Nginx (..), NginxConfig (..))
import SqlConfig (Create (..), DBColumnDescription (..), DBType (..), SQLExpression (..))
import System.Directory
import Text.Megaparsec as MP
import Text.Megaparsec.Char (char, letterChar, numberChar, printChar, spaceChar, string)
import Text.Megaparsec.Debug (dbg)
import Text.Pretty.Simple
import Text.Printf
import Text.Read (readMaybe)

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

data DBEntry
  = EString String
  | EDateTime UTCTime
  | EFloat32 Float
  | EFixedString Int String
  | EInt32 Int
  | EInt64 Int
  | EDate UTCTime
  deriving (Show)

type MParser = Parsec Void String

data FormatToken = TWord {lParen :: String, magicWord :: String, rParen :: String} | TDelim String deriving (Show)

pMagicWord :: MParser String
pMagicWord = do
  _ <- string "$"
  some (letterChar <|> numberChar <|> char '_')

lparens = ['(', '[', '"', '\'']

rparens = [')', ']', '"', '\'']

delims = ['-']

pFormatToken :: MParser FormatToken
pFormatToken = do
  choice
    [ do
        l <- many $ oneOf lparens
        w <- pMagicWord
        r <- many $ oneOf rparens
        return $ TWord l w (r <> " "),
      do
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

logLine1 :: String
logLine1 = "213.158.8.97 - - [06/Apr/2022:00:17:01 +0300] 'GET /v2/rt-wink/live/playlist?key=00000141-1a10-726f-7374-656c65636f6d&client_time=1649193421&channel_id=3541715&san=10215020191125&client_id=fc%3A44%3A9f%3A36%3A03%3Adb&location=200004&mcast=igmp%3A%2F%2F225.78.32.76%3A5000&event_id=00000000-0000-0000-0000-000000000001&age_value=18 HTTP/1.1' 'GET' '-' '0.002' 200 401 '-' 'RT-STB-FW/7.46.4 (zte_amls805, B700V7L) stbapp/1.54.4-gd649a14ff' '0.002' 'north-west.rt.getshop.tv' 'spb-1' '1.1 rt-nw-hub-proxy01 (squid/4.8)' 'fc:44:9f:36:03:db' '089D73ED-1045-4839-8D82-AC46662612FD' '10215020191125' '{\x22device_timestamp\x22:1.649193421468805927e9,\x22events\x22:[{\x22banner\x22:null,\x22id\x22:\x22 00000000-0000-0000-0000-000000000001\x22,\x22type\x22:\x22playlist_update\x22,\x22datetime\x22:1.649199847477686405181e9}]}' '-' '3541715' '-' '-' 'fc%3A44%3A9f%3A36%3A03%3Adb' '-' '-' '-' '-' '00000000-0000-0000-0000-000000000001' '200004' '10215020191125'"

-- pAnyUntil :: String -> MParser String
-- pAnyUntil s = some (printChar <* lookAhead (string s))

pBetween :: String -> String -> String -> MParser String
pBetween lp w rp = do
  _ {-dbg "left paren" $ -} <- string lp
  s {-dbg "words" $ -} <- takeWhileP Nothing (/= head rp)
  _ {-dbg "right paren" $ -} <- string rp
  return s

pDelim :: String -> MParser String
pDelim s = {-dbg "delim" $ -} string (s ++ " ")

tokenizeLine :: [FormatToken] -> MParser [(MagicWord, String)]
tokenizeLine fs = do
  p <- forM fs (\case TWord lp w rp -> pBetween lp w rp; TDelim s -> pDelim s)
  let ws = (\case TWord _ w _ -> w; TDelim w -> w) <$> fs
  return $ zip ws p

pDBDateTime :: String -> Maybe DBEntry
pDBDateTime s = EDateTime <$> parseTimeM True defaultTimeLocale "%d/%b/%0Y:%H:%M:%S %z" s

pDBDate :: String -> Maybe DBEntry
pDBDate s = EDate <$> parseTimeM True defaultTimeLocale "%d/%b/%0Y" s

pDBFloat32 :: String -> Maybe DBEntry
pDBFloat32 s = EFloat32 <$> readMaybe s

pDBInt32 :: String -> Maybe DBEntry
pDBInt32 s = EInt32 <$> readMaybe s

pDBString :: String -> Maybe DBEntry
pDBString s = Just $ EString s

pDBInt64 :: String -> Maybe DBEntry
pDBInt64 s = EInt64 <$> readMaybe s

pDBFixedString :: Int -> String -> Maybe DBEntry
pDBFixedString i s = if length s <= i then Just (EString s) else Nothing

data GetParser = FromString String | FromIntString Int String

getParsers :: MagicWord -> GetParser -> Maybe DBEntry
getParsers w g =
  case g of
    FromString s
      | w
          `elem` [ "remote_addr",
                   "remote_user",
                   "request",
                   "http_referer",
                   "http_user_agent",
                   "request_method",
                   "https",
                   "host",
                   "hostname",
                   "request_body",
                   "resp_body",
                   "arg_campaign_id",
                   "arg_channel_id",
                   "arg_channel_name",
                   "arg_chmap",
                   "arg_client_id",
                   "arg_client_session_id",
                   "arg_client_version",
                   "arg_display_height",
                   "arg_display_width",
                   "arg_event_id",
                   "arg_location",
                   "arg_san",
                   "req_header_via",
                   "req_header_x_rt_mac",
                   "req_header_x_rt_san",
                   "req_header_x_rt_uid"
                 ] ->
        pDBString s
      | w == "time_local" -> pDBDateTime s
      | w
          `elem` [ "request_time",
                   "upstream_connect_time",
                   "upstream_header_time",
                   "upstream_response_time",
                   "msec"
                 ] ->
        pDBFloat32 s
      | w
          `elem` [ "bytes_sent",
                   "connections_waiting",
                   "connections_active",
                   "status",
                   "connection",
                   "request_length",
                   "body_bytes_sent"
                 ] ->
        pDBInt32 s
      | otherwise -> Nothing
    FromIntString i s -> pDBFixedString i s

{-
>>>pDateTime "06/Apr/2022:00:17:01 +0300"
Just (EDateTime 2022-04-05 21:17:01 UTC)

>>>pDate "06/Apr/2022"
Just (EDate 2022-04-06 00:00:00 UTC)
-}

-- | pretty-prints a map Column name -> value
main :: IO ()
main = do
  (nginxConfig :: Either ParseException NginxConfig.NginxConfig) <- decodeFileEither nginxConfigPath
  (sqlConfig :: Either ParseException [SQLExpression]) <- decodeFileEither sqlConfigPath
  -- print logLine1
  let p = matchDBTypesFromSQL <$> nginxConfig <*> (head <$> sqlConfig)
  -- pPrint p
  let f = tokenizeFormat <$> nginxConfig
  -- pPrint f
  case (f, p) of
    (Right (Just f'), Right p') -> do
      -- print logLine
      let ts = parseMaybe (tokenizeLine f') (logLine1 <> " ")
      case ts of
        Just l -> do
          let ms = fromList l
              -- assume that magic words decide the entry type
              ts' =
                HashMap.fromList $
                  ( \(ColumnMatch c w t) ->
                      ( c,
                        HashMap.lookup w ms
                          >>= (\s -> getParsers w (FromString s))
                      )
                  )
                    <$> p'
          pPrint ts'
          return ()
        _ -> print "oops"
      return ()
    _ -> print "oops"
  -- print $ ((`parseMaybe` logLine) . tokenizeLine <$>) <$> f
  -- pPrint nginxConfig
  -- pPrint sqlConfig
  return ()

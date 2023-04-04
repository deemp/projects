{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# HLINT ignore "Fuse on/on" #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib (main) where

import Control.Monad (forM_, void)
import Control.Monad.Logger (LogLevel (..), LoggingT, filterLogger, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT (..))
import Data.Aeson (Options (..), defaultOptions, genericParseJSON)
import Data.Data (Proxy (..))
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Text.IO.Utf8 ()
import Data.Yaml
import Database.Esqueleto.Experimental (BackendKey (..), FieldDef (..), PersistEntity, PersistStoreWrite (insert), SqlPersistT, Value (..), delete, desc, from, innerJoin, on, orderBy, runMigration, select, table, val, where_, (=.), (==.), (>.), (^.), type (:&) ((:&)))
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)
import Main.Utf8 (withUtf8)
import Text.Pretty.Simple (pPrint)

-- define schema
-- TODO add cascade
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Author
    name_author String
    deriving Eq Show
  Genre
    name_genre String
    deriving Eq Show
  Book
    title String
    author_id AuthorId
    genre_id GenreId
    price Double
    amount Double
    deriving Eq Show
|]

-- authorId is a foreign key

type Action a = SqlPersistT (LoggingT IO) a

data DBConfig = DBConfig
  { host :: String
  , port :: Int
  , user :: String
  , dbname :: String
  , password :: String
  }
  deriving (Generic)

aesonOptions :: Options
aesonOptions = defaultOptions

instance FromJSON DBConfig where
  parseJSON :: Data.Yaml.Value -> Parser DBConfig
  parseJSON = genericParseJSON aesonOptions

type PGInfo = ConnectionString

conn :: DBConfig -> PGInfo
conn DBConfig{..} = [i|host=#{host} port=#{port} user=#{user} dbname=#{dbname} password=#{password}|]

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError = True
logFilter _ LevelWarn = True
logFilter _ LevelInfo = True
logFilter _ LevelDebug = True
logFilter _ (LevelOther _) = True

runAction_ :: PGInfo -> Action a -> IO a
runAction_ connectionString action =
  runStdoutLoggingT $
    filterLogger logFilter $
      withPostgresqlConn connectionString $ \backend ->
        runReaderT action backend

runAction :: DBConfig -> Action a -> IO a
runAction config = runAction_ (conn config)

selectAllAction :: Action [(String, String, Double)]
selectAllAction =
  do
    res <- select do
      (genre :& book :& _) <-
        from
          $ table @Genre
            `innerJoin` table @Book
          `on` ( \(genre :& book) ->
                  genre ^. GenreId ==. book ^. BookGenre_id
               )
            `innerJoin` table @Author
          `on` ( \(_ :& book :& author) ->
                  author ^. AuthorId ==. book ^. BookAuthor_id
               )
      where_ (book ^. BookAmount >. val 8)
      orderBy [desc (book ^. BookPrice)]
      return (book ^. BookTitle, genre ^. GenreName_genre, book ^. BookPrice)
    return $ (\(x, y, z) -> (unValue x, unValue y, unValue z)) <$> res

deleteAll :: Action ()
deleteAll =
  do del @Book >> del @Genre >> del @Author
 where
  del :: forall a. PersistEntity a => Action ()
  del = delete $ void $ from $ table @a

main :: IO ()
main = withUtf8 do
  config <- decodeFileThrow "config.yaml"
  books <- runAction config do
    deleteAll
    runMigration migrateAll

    -- insert authors
    aBulgakov <- insert $ Author "Булгаков М.А."
    aDostoyevsky <- insert $ Author "Достоевский Ф.М."
    aYesenin <- insert $ Author "Есенин С.А."
    aPasternak <- insert $ Author "Пастернак Б.Л."
    _ <- insert $ Author "Лермонтов М.Ю."

    -- insert genres
    gRoman <- insert $ Genre "Роман"
    gPoeziya <- insert $ Genre "Поэзия"
    _ <- insert $ Genre "Приключения"

    -- insert books
    for_
      [ Book "Мастер и Маргарита" aBulgakov gRoman 670.99 3
      , Book "Белая гвардия" aBulgakov gRoman 540.50 5
      , Book "Идиот" aDostoyevsky gRoman 460.00 10
      , Book "Братья Карамазовы" aDostoyevsky gRoman 799.01 3
      , Book "Игрок" aDostoyevsky gRoman 480.50 10
      , Book "Стихотворения и поэмы" aYesenin gPoeziya 650.00 15
      , Book "Черный человек" aYesenin gPoeziya 570.20 6
      , Book "Лирика" aPasternak gPoeziya 518.99 2
      ]
      insert
    selectAllAction

  forM_ books pPrint

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lib (main) where

import Control.Monad (forM_)
import Control.Monad.Logger (LogLevel (..), LoggingT, filterLogger, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT (..))
import Data.Foldable (for_)
import Data.Traversable (for)
import Database.Esqueleto.Experimental
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

-- define schema
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
    authorId AuthorId
    genreId GenreId
    price Double
    amount Double
    deriving Eq Show
|]

-- authorId is a foreign key

type Action a = SqlPersistT (LoggingT IO) a

authors :: [Author]
authors =
  [ Author "Булгаков М.А."
  , Author "Достоевский Ф.М."
  , Author "Есенин С.А."
  , Author "Пастернак Б.Л."
  , Author "Лермонтов М.Ю."
  ]

genres :: [Genre]
genres =
  [ Genre "Роман"
  , Genre "Поэзия"
  , Genre "Приключения"
  ]

selectAllAction :: Action [Book]
selectAllAction = (entityVal <$>) <$> (select $ from $ table @Book)

type PGInfo = ConnectionString

conn :: PGInfo
conn = "host=192.168.58.2 port=30002 user=admin dbname=postgresdb password=psltest"

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError = True
logFilter _ LevelWarn = True
logFilter _ LevelInfo = True
logFilter _ LevelDebug = False
logFilter _ (LevelOther _) = False

runAction_ :: PGInfo -> Action a -> IO a
runAction_ connectionString action =
  runStdoutLoggingT $
    filterLogger logFilter $
      withPostgresqlConn connectionString $ \backend ->
        runReaderT action backend

runAction :: Action a -> IO a
runAction = runAction_ conn

main :: IO ()
main = do
  books <- runAction do
    runMigration migrateAll
    [aBulgakov, aDostoyevsky, aYesenin, aPasternak, aLermontov] <- for authors insert
    [gRoman, gPoeziya, gPriklucheniya] <- for genres insert
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
  forM_ books print
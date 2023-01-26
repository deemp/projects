{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Fuse on/on" #-}
{-# LANGUAGE TupleSections #-}

module Lib (main) where

import Control.Monad (forM_)
import Control.Monad.Logger (LogLevel (..), LoggingT, filterLogger, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT (..))
import Data.Foldable (for_)
import Data.Text.IO.Utf8 ()
import Database.Esqueleto.Experimental
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Main.Utf8 (withUtf8)
import Text.Pretty.Simple (pPrint)
import qualified Data.Text.IO as LBS

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

-- TODO useful
-- entityVal

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

type PGInfo = ConnectionString

conn :: PGInfo
conn = "host=192.168.58.2 port=30002 user=admin dbname=postgresdb password=psltest"

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

runAction :: Action a -> IO a
runAction = runAction_ conn

-- privet = T.pack "Привет"

-- >>>privet
-- "\1055\1088\1080\1074\1077\1090"

main :: IO ()
main = withUtf8 do
  books <- runAction do
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

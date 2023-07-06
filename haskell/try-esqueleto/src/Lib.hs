{-# HLINT ignore "Fuse on/on" #-}

module Lib where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, void)
import Control.Monad.Logger (LogLevel (..), LoggingT, filterLogger, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT (..))
import Data.Aeson (Options (..), defaultOptions, genericParseJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (for_)
import Data.String.Interpolate (i)
import Data.Text.IO.Utf8 ()
import Data.Yaml
import Database.Esqueleto.Experimental (BackendKey (..), FieldDef (..), PersistEntity, PersistStoreWrite (insert), SqlPersistT, Value (..), delete, desc, from, innerJoin, on, orderBy, putMany, runMigration, select, table, val, where_, (==.), (>.), type (:&) ((:&)))
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
    name String
    password String
    UniqueName name
    deriving Eq Show
  Genre
    name String
    deriving Eq Show
  Book
    title String
    authorId AuthorId
    genreId GenreId
    price Double
    amount Double
    UniqueTitle title
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

select1 :: Action [(String, String, Double)]
select1 =
  do
    res <- select do
      (genre :& book :& _) <-
        from
          $ table @Genre
            `innerJoin` table @Book
          `on` ( \(genre :& book) ->
                  genre.id ==. book.genreId
               )
            `innerJoin` table @Author
          `on` ( \(_ :& book :& author) ->
                  author.id ==. book.authorId
               )
      where_ (book.amount >. val 8)
      orderBy [desc (book.price)]
      pure (book.title, genre.name, book.price)
    pure $ (\(x, y, z) -> (unValue x, unValue y, unValue z)) <$> res

update1 :: Action [(String, String)]
update1 = do
  putMany [Author "Булгаков М.А." "anotherPassword"]
  authors <- select do
    authors <- from $ table @Author
    pure (authors.name, authors.password)
  pure $ bimap unValue unValue <$> authors

deleteAll :: Action ()
deleteAll =
  do del @Book >> del @Genre >> del @Author
 where
  del :: forall a. PersistEntity a => Action ()
  del = delete $ void $ from $ table @a

mkAuthor :: String -> Author
mkAuthor name = Author name "password"

prepareTable :: Action ()
prepareTable = do
  runMigration migrateAll

  -- insert authors
  aBulgakov <- insert $ mkAuthor "Булгаков М.А."
  aDostoyevsky <- insert $ mkAuthor "Достоевский Ф.М."
  aYesenin <- insert $ mkAuthor "Есенин С.А."
  aPasternak <- insert $ mkAuthor "Пастернак Б.Л."
  _ <- insert $ mkAuthor "Лермонтов М.Ю."

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

main :: IO ()
main = withUtf8 do
  config <- decodeFileThrow "config.yaml"
  let runAction' :: Action a -> IO a
      runAction' = runAction config
  deletionResult :: Either SomeException () <- try $ runAction' deleteAll
  either
    (\err -> putStrLn $ "\n\nSkipping deletion: \n\n" <> show err)
    (const $ pure ())
    deletionResult
  runAction' prepareTable
  putStrLn "\n\nQueries\n\n"
  runAction' select1 >>= \books -> forM_ books pPrint
  runAction' update1 >>= \authors -> forM_ authors pPrint

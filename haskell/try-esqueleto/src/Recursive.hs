module Recursive where

import Control.Exception (SomeException, catch)
import Data.Functor
import Data.Text.IO.Utf8 ()
import Data.Yaml
import Database.Esqueleto.Experimental (BackendKey (..), Entity (entityVal), FieldDef (..), PersistStoreWrite (insert), from, innerJoin, just, on, runMigration, select, table, unionAll_, val, where_, withRecursive, (==.), type (:&) ((:&)))
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Lib (Action, runAction)
import Main.Utf8 (withUtf8)
import Text.Pretty.Simple (pPrint)

-- define schema
-- TODO add cascade
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Employee
    name String
    manager EmployeeId Maybe
    UniqueEmployeeName name
    deriving Ord Eq Show
|]

prepareTable :: Action ()
prepareTable = do
  runMigration migrateAll
  e1 <- insert $ Employee "1" Nothing
  e2 <- insert $ Employee "2" (Just e1)
  _ <- insert $ Employee "3" (Just e2)
  _ <- insert $ Employee "4" Nothing
  pure ()

select1 :: Action [Employee]
select1 = do
  e :: [Entity Employee] <- select do
    e <-
      withRecursive
        ( do
            employees <- from $ table @Employee
            where_ $ employees.name ==. val "1"
            pure employees
        )
        unionAll_
        ( \self -> do
            (emp :& _) <-
              from
                $ table @Employee
                  `innerJoin` self
                `on` (\(emp :& empPrev) -> emp.manager ==. just empPrev.id)
            pure emp
        )
    from e
  pure $ e <&> entityVal

main :: IO ()
main = withUtf8 do
  config <- decodeFileThrow "config.yaml"
  let runAction' :: Action a -> IO a
      runAction' = runAction config
  runAction' prepareTable `catch` (\(e :: SomeException) -> print e)
  putStrLn "\n\nQueries\n\n"
  runAction' select1 >>= mapM_ pPrint

{-
# Simplex-cheat

We're going to implement a simplex-like chat app.
It will have a server and bots talking to each other via that server.

## Language extensions

We'll need the following language extensions and pragmas:
-}

{- FOURMOLU_DISABLE -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{- FOURMOLU_ENABLE -}

{-

## Imports

Next, we can declare our module and write some imports
-}

module Lib (main) where

import Control.Concurrent (
  ThreadId,
  forkIO,
  threadDelay,
  throwTo,
 )
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Concurrent.STM (
  TQueue,
  TVar,
  atomically,
  flushTQueue,
  modifyTVar,
  modifyTVar',
  newTQueue,
  newTQueueIO,
  newTVarIO,
  readTQueue,
  readTVarIO,
  writeTQueue,
  writeTVar,
 )
import Control.Exception (SomeException, bracketOnError)
import Control.Monad (forM_, forever, replicateM, unless, when)
import Control.Monad.Catch (Exception (toException))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), asks)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Aeson.Key (fromString, toText)
import Data.Aeson.KeyMap (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Tuple (swap)
import Data.Yaml (ParseException)
import Data.Yaml.Aeson (FromJSON (parseJSON), Parser, ToJSON, Value, decodeFileEither, decodeFileThrow, encodeFile, withObject, (.:))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Capture, Get, Handler, HasServer (ServerT), JSON, Post, Proxy (..), ReqBody, ServerError (..), err404, serve, throwError, type (:<|>) (..), type (:>))
import Servant.Client (BaseUrl (BaseUrl), ClientError, ClientM, Scheme (..), client, mkClientEnv)
import Servant.Client.Internal.HttpClient (runClientM)
import Servant.Server (hoistServer)
import StmContainers.Map (Map, delete, insert, lookup, newIO)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath (takeDirectory)
import System.Random.Shuffle (shuffle')
import System.Random.Stateful (StdGen, globalStdGen, initStdGen, uniformRM)
import Prelude hiding (log, lookup)

{-

## Implementation

Now, we can proceed to the implementation of the app.

There are four main entities in our app:
- server
- spawner
- user
- logger

They're configured via configs in the [configs](./data/configs) dir

On a server, there will be a pair of message queues for each user pair
- A queue for Alice where she receives messages and where Bob can send his messages to
- A symmetric queue for Bob

The server doesn't need to know that these queues belong to Alice and Bob.
It will just protect these queues with a secret key and tell it to Alice and Bob.
It's not a problem that Alice can read messages from that particular Bob's queue as these are the messages that Alice sent herself

A `server` will handle the requests:
- Make a chat for Alice and Bob
  1. Create queues
  1. Generate a secret key
  1. Write queue ids and a secret key into a specified file
- Accept a message sent by Alice and put it into Bob's queue
  1. Check if Alice's key is valid for Bob's queue

A `spawner` will:
- Observe the file with [contacts](./data/configs/contacts.yaml) config
- Handle its changes
- It's assumed that if there appears in the config an `Alice.Bob` object, there also appears the same `Bob.Alice` object
- If there appears `Alice.Bob`, `spawner` will
  - if `Alice` doesn't exist:
      1. spawn `Alice` and connect to her via a `Queue` (a queue with contacts)
      1. Put Bob's contact containg the path to their chat config into that queue
  - else update the `Bob`'s contact in `Alice`'s `Queue`
- If a new config doesn't have Alice, a spawner will kill her thread
- Request to remove stale queues

A `user` like `Alice` will:
- send requests to the server to make a chat config ([example](./data/configs/alice_bob.yaml))
  - she should provide the filename where to write that config
- take contacts from her contacts queue
  - if a contact's file doesn't exist
    - it may haven't yet been created, so ask the server to create such a file and decrement the number of queries left
    - if the `contacts config` doesn't have this file, the spawner will remove the file
    - so, eventually, there will be no queries left and the contact will be dropped from Alice's queue
- send messages to Bob's queue and to queues of other contacts
- log received messages
- drop a contact then

A `logger` will:
- read logs from a shared queue with logs
- print them

With a shared log queue, the logs of all entities will come one-by-one to stdout
-}

newtype Name = Name Text
  deriving (Generic, Eq, FromJSON, ToJSON)
  deriving anyclass (Hashable)

instance Show Name where
  show :: Name -> String
  show (Name name) = Text.unpack name

newtype MessageMakeChat = MessageMakeChat
  { file :: FilePath
  }
  deriving (Generic, FromJSON, ToJSON)

data MessageSend = MessageSend
  { contents :: Text
  , sentAt :: UTCTime
  , key :: SecretKey
  }
  deriving (Generic, FromJSON, ToJSON)

data MessageReceive = MessageReceive
  { contents :: Text
  , sentAt :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON)

data MessagesReceive = MessagesReceive
  { -- when server sent message to a receiver
    serverSentAt :: UTCTime
  , messages :: [MessageReceive]
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON)

type QueueID = Int

data MessageRemoveQueues = MessageRemoveQueues
  { queues :: [QueueID]
  }
  deriving (Generic, FromJSON, ToJSON)

type MakeChat =
  "make"
    :> ReqBody '[JSON] MessageMakeChat
    :> Post '[JSON] ()

type SendMessage =
  "send"
    :> Capture "id" QueueID
    :> ReqBody '[JSON] MessageSend
    :> Post '[JSON] ()

type ReceiveMessages =
  "receive"
    :> Capture "id" QueueID
    :> Capture "key" SecretKey
    :> Get '[JSON] MessagesReceive

type RemoveQueues =
  "remove"
    :> ReqBody '[JSON] MessageRemoveQueues
    :> Post '[JSON] ()

type SecretKey = Text

api :: Proxy Api
api = Proxy

type Api =
  "server"
    :> SendMessage
    :<|> ReceiveMessages
    :<|> MakeChat
    :<|> RemoveQueues

sendMessage :: QueueID -> MessageSend -> ClientM ()
receiveMessage :: QueueID -> SecretKey -> ClientM MessagesReceive
makeChat :: MessageMakeChat -> ClientM ()
removeQueues :: MessageRemoveQueues -> ClientM ()
sendMessage :<|> receiveMessage :<|> makeChat :<|> removeQueues = client api

data ConfigValue = ConfigValue
  { file :: FilePath
  , starts :: Name
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype ContactsConfig = ContactsConfig {unContactsConfig :: HM.HashMap (Name, Name) ConfigValue}
  deriving (Generic)

instance FromJSON ContactsConfig where
  parseJSON :: Value -> Parser ContactsConfig
  parseJSON =
    withObject
      "Parent"
      ( \parent ->
          do
            let mkConfigValue k1 k2 =
                  withObject
                    "ConfigValue"
                    ( \cv ->
                        return $
                          ((Name $ toText k1, Name $ toText k2),)
                            <$> ( ConfigValue
                                    <$> (cv .: fromString "file")
                                    <*> (cv .: fromString "starts")
                                )
                    )
                k1v1 =
                  toList parent
                    <&> ( \(k1, v1) ->
                            withObject
                              "Child"
                              ( \child ->
                                  sequenceA (toList child <&> uncurry (mkConfigValue k1))
                                    >>= sequenceA
                                    <&> HM.fromList
                              )
                              v1
                        )
            ContactsConfig . HM.unions <$> sequenceA k1v1
      )

data Starts = Self | Another

data Contact = Contact
  { name :: Name
  , file :: FilePath
  , starts :: Starts
  , retryTimes :: Int
  , messageNumber :: Int
  }

data User = User
  { threadId :: ThreadId
  , queue :: TQueue Contact
  }

data UserError
  = NotInConfigError
  deriving (Show, Exception)

type Log = TQueue Text

-- | Logs stuff from the queue
runLogger :: Log -> IO ()
runLogger log_ = forever do
  liftIO $ atomically (readTQueue log_) >>= T.putStrLn

data ClientEnv = ClientEnv
  { log :: Log
  , manager :: Manager
  , _CONTACTS_CONFIG_PATH :: FilePath
  , _RETRY_TIMES :: Int
  , _DELAY_CONTACT_MIN :: Double
  , _DELAY_CONTACT_MAX :: Double
  , _DELAY_SPAWNER :: Double
  , _LOG_ENABLED :: Bool
  , _PORT :: Int
  }

type App env a = ReaderT env IO a

logText :: HasLog env => Text -> App env ()
logText t = do
  log_ <- asks getLog
  liftIO $ atomically $ writeTQueue log_ t

runReaderT' :: b -> ReaderT b m a -> m a
runReaderT' = flip runReaderT

class HasLog a where
  getLog :: a -> Log
  getLogEnabled :: a -> Bool

instance HasLog ClientEnv where
  getLog :: ClientEnv -> Log
  getLog e = e.log
  getLogEnabled :: ClientEnv -> Bool
  getLogEnabled e = e._LOG_ENABLED

writeLog :: (HasLog env, MonadReader env m, MonadIO m) => Text -> m ()
writeLog msg = do
  logEnabled_ <- asks getLogEnabled
  when logEnabled_ do
    log_ <- asks getLog
    liftIO $ atomically $ writeTQueue log_ msg

writeLogConsecutive :: (HasLog env, MonadReader env m, MonadIO m) => [Text] -> m ()
writeLogConsecutive msgs = do
  logEnabled_ <- asks getLogEnabled
  when logEnabled_ do
    log_ <- asks getLog
    liftIO $ atomically $ forM_ msgs (writeTQueue log_)

type Names = (Name, Name)

runSpawner :: App ClientEnv ()
runSpawner = do
  config <- liftIO $ newTVarIO HM.empty
  users <- liftIO $ newTVarIO (HM.empty :: HM.HashMap Names User)
  let spawnerB = br $ Name "Spawner"
  env <- ask
  forever do
    liftIO $ sleepRandom env._DELAY_SPAWNER (env._DELAY_SPAWNER + 1)
    res :: Either SomeException () <- runExceptT do
      configPath <- asks _CONTACTS_CONFIG_PATH
      configOld <- liftIO $ readTVarIO config
      configNew <- unContactsConfig <$> decodeFileThrow configPath
      liftIO $ atomically $ writeTVar config configNew
      let
        diffOld = HM.toList $ HM.difference configOld configNew
        diffNew = HM.toList $ HM.difference configNew configOld
      -- remove users that don't exist in the new config
      users' <- liftIO $ readTVarIO users
      liftIO $
        forM_
          diffOld
          ( \(names, ConfigValue{..}) -> do
              maybe'
                (HM.lookup names users')
                (return ())
                ( \User{..} -> do
                    throwTo threadId NotInConfigError
                    doesFileExist file
                      >>= ( `when`
                              runReaderT' env do
                                -- request to remove the queues
                                chatConfig :: Either ParseException ChatConfig <- liftIO $ decodeFileEither file
                                either'
                                  chatConfig
                                  (\err -> writeLog [i|#{spawnerB} couldn't parse the chat config: #{err}|])
                                  ( \conf -> do
                                      let queues = [conf.starts, conf.another]
                                      res <- liftIO $ req_ env.manager (removeQueues MessageRemoveQueues{queues})
                                      either'
                                        res
                                        (\err -> writeLog [i|#{spawnerB} unsuccessfully requested to remove queues: #{err}|])
                                        (\_ -> writeLog [i|#{spawnerB} successfully requested to remove queues: #{queues}|])
                                  )
                                liftIO $ removeFile file
                          )
                    liftIO $ atomically $ modifyTVar users (HM.delete names)
                )
          )
      forM_
        diffNew
        ( \(names@(name1, name2), ConfigValue{..}) -> do
            let sendContact queue =
                  atomically $
                    writeTQueue
                      queue
                      Contact
                        { name = name2
                        , file
                        , starts = if name1 == starts then Self else Another
                        , retryTimes = env._RETRY_TIMES
                        , messageNumber = 0
                        }
            maybe'
              (HM.lookup names users')
              ( -- if a user doesn't exist
                do
                  -- spawn a user
                  newQueue <- liftIO newTQueueIO
                  writeLog [i|#{spawnerB} created a user: #{br name1} for #{br name2}|]
                  threadId_ <- liftIO $ forkIO $ runReaderT' env (runUser name1 newQueue)

                  -- record new user
                  liftIO $
                    atomically $
                      modifyTVar'
                        users
                        ( HM.insert names User{threadId = threadId_, queue = newQueue}
                        )

                  -- send a contact to that user
                  liftIO $ sendContact newQueue
              )
              ( -- if a user exists
                \User{..} -> do
                  -- send a contact to that user
                  liftIO $ sendContact queue
              )
        )
    either'
      res
      (\x -> writeLog [i|#{spawnerB} error: #{x}|])
      return

class Show a => ShowBracketed a where
  br :: a -> String

instance Show a => ShowBracketed a where
  br :: a -> String
  br x = [i|[ #{x} ]|]

data ChatConfig = ChatConfig
  { starts :: Int
  -- ^ ID of a queue of a user that started the chat
  , another :: Int
  -- ^ ID of a queue of another user
  , key :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

either' :: Either a b -> (a -> c) -> (b -> c) -> c
either' x f g = either f g x

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' val x y = maybe x y val

seconds :: Double -> Int
seconds n = ceiling (n * 1000000)

-- | thread delay for a random number of seconds between lower_bound and upper_bound
sleepRandom ::
  -- | lower_bound
  Double ->
  -- | upper_bound
  Double ->
  IO ()
sleepRandom mini maxi =
  threadDelay =<< (uniformRM ((mini `seconds`), (maxi `seconds`)) globalStdGen :: IO Int)

newtype ContactError = ContactUnavailable {name :: Name}
  deriving (Exception)

instance Show ContactError where
  show :: ContactError -> String
  show ContactUnavailable{..} = [i|Contact #{br name} is unavailable|]

req_ :: Manager -> ClientM a -> IO (Either ClientError a)
req_ manager clientM = do
  let baseUrl_ = BaseUrl Http "localhost" 8080 ""
  liftIO $ runClientM clientM (mkClientEnv manager baseUrl_)

runUser :: Name -> TQueue Contact -> App ClientEnv ()
runUser selfName queue = do
  env <- ask
  let req = req_ env.manager
      selfB = br selfName
      delayContact = liftIO $ sleepRandom env._DELAY_CONTACT_MIN env._DELAY_CONTACT_MAX
  forever do
    delayContact
    res :: Either SomeException () <- runExceptT do
      contact <- liftIO $ atomically $ readTQueue queue
      let anotherB = br contact.name
      existsFile <- liftIO $ doesFileExist contact.file
      unless existsFile do
        if contact.retryTimes > 0
          then do
            case contact.starts of
              Self -> do
                res <- liftIO $ req (makeChat MessageMakeChat{file = contact.file})
                either
                  (\err -> writeLog [i|#{selfB} error when requesting a connection with #{anotherB}: #{br err}|])
                  (\_ -> writeLog [i|#{selfB} sent a request to connect with #{anotherB}|])
                  res
              Another -> do
                writeLog [i|#{selfB} waits until #{anotherB} sends a request to connect|]
            -- TODO decide where to write updated contact
            let updatedContact = contact{retryTimes = contact.retryTimes - 1}
            liftIO $ atomically $ writeTQueue queue updatedContact
            throwE $ toException (ContactUnavailable contact.name)
          else writeLog [i|#{selfB} was unable to connect with #{anotherB}. No attempts left|]
      config :: ChatConfig <- liftIO $ decodeFileThrow contact.file
      let (sendTo, receiveFrom) =
            (config.starts, config.another)
              & case contact.starts of
                Self -> swap
                Another -> id
      sendMessageRes <-
        liftIO do
          curTime <- getCurrentTime
          req
            ( sendMessage
                sendTo
                MessageSend
                  { contents = [i|#{selfB}: #{contact & messageNumber}|]
                  , sentAt = curTime
                  , key = config.key
                  }
            )
      either'
        sendMessageRes
        ( \err -> writeLog [i|#{selfB} was unable to send a message to #{anotherB}: #{err}|]
        )
        ( \_ -> do
            writeLog [i|#{selfB} sent a message \##{contact & messageNumber} to #{anotherB}|]
            liftIO $ atomically $ writeTQueue queue contact{messageNumber = contact.messageNumber + 1}
        )
      receiveMessagesRes <- liftIO $ req (receiveMessage receiveFrom config.key)
      either'
        receiveMessagesRes
        ( \err -> writeLog [i|#{selfB} was unable to receive messages from #{anotherB}: #{err}|]
        )
        ( \messages -> do
            let messages_ :: [Text] = (\x -> x.contents) <$> messages.messages
            writeLogConsecutive ([i|#{selfB} received messages from #{anotherB}: |] : messages_)
        )
    either'
      res
      ( \err -> writeLog [i|#{selfB} got an error when processing the contacts queue: #{err}|]
      )
      return

data RandomSeq = RandomSeq
  { blockSize :: Int
  , upperBound :: Int
  , block :: [Int]
  }

initRandomSeq :: Int -> Maybe RandomSeq
initRandomSeq blockSize
  | blockSize < 10 = Nothing
  | otherwise =
      Just
        RandomSeq
          { blockSize
          , upperBound = 0
          , block = []
          }

nextRandomNumber :: StdGen -> RandomSeq -> (Int, RandomSeq)
nextRandomNumber gen r =
  case r.block of
    -- head guarranteed by the constructor
    [] -> (head s, r{block = tail s, upperBound = r.upperBound + r.blockSize})
     where
      s = shuffle' [r.upperBound + 1 .. r.blockSize] r.blockSize gen
    x -> (head x, r{block = tail x})

data ServerEnv = ServerEnv
  { stdGen :: StdGen
  , randomSeq :: TVar RandomSeq
  , _KEY_LENGTH :: Int
  , queues :: Map QueueID Queue
  , log :: Log
  , _LOG_ENABLED :: Bool
  }

instance HasLog ServerEnv where
  getLog :: ServerEnv -> Log
  getLog e = e.log
  getLogEnabled :: ServerEnv -> Bool
  getLogEnabled e = e._LOG_ENABLED

data Queue = Queue
  { queue :: TQueue MessageSend
  , key :: Text
  }

type ServerM = ReaderT ServerEnv Handler

nt :: ServerEnv -> ServerM a -> Handler a
nt s x = runReaderT x s

app :: ServerEnv -> Application
app s = serve api $ hoistServer api (nt s) runServer

runServer :: ServerT Api ServerM
runServer = handleSendMessage :<|> handleReceiveMessage :<|> handleMakeChat :<|> handleRemoveQueues
 where
  server_ = "[ Server ]" :: String
  handleMakeChat :: MessageMakeChat -> ServerM ()
  handleMakeChat MessageMakeChat{..} = do
    let dir = takeDirectory file
    env <- ask
    liftIO $ createDirectoryIfMissing True dir
    gen <- asks stdGen
    randomSeq_ <- liftIO =<< asks (readTVarIO . randomSeq)
    let (id1, randomSeq1) = nextRandomNumber gen randomSeq_
        (id2, randomSeq2) = nextRandomNumber gen randomSeq1
    randomSeq' <- asks randomSeq
    liftIO $ atomically $ modifyTVar randomSeq' (const randomSeq2)
    key_ <- T.pack <$> replicateM env._KEY_LENGTH (uniformRM ('a', 'z') globalStdGen)
    let chatConfig = ChatConfig{starts = id1, another = id2, key = key_}
    liftIO $ encodeFile file chatConfig
    liftIO $ atomically do
      queue1 <- newTQueue
      insert Queue{queue = queue1, key = key_} id1 env.queues
      queue2 <- newTQueue
      insert Queue{queue = queue2, key = key_} id2 env.queues
    writeLog [i|#{server_} created queues with ids: #{id1} and #{id2}|]
  handleSendMessage :: QueueID -> MessageSend -> ServerM ()
  handleSendMessage qid msg = do
    env <- ask
    queue_ <- liftIO $ atomically $ lookup qid env.queues
    maybe'
      queue_
      (throwError err404{errBody = [i|Sorry, there is no queue #{qid}|]})
      ( \q@Queue{..} -> do
          when
            (q.key /= msg.key)
            (throwError err404{errBody = [i|You can't send messages to the queue #{qid}|]})
          liftIO $ atomically $ writeTQueue queue msg
      )
  handleReceiveMessage :: QueueID -> SecretKey -> ServerM MessagesReceive
  handleReceiveMessage qid key = do
    env <- ask
    queue_ <- liftIO $ atomically $ lookup qid env.queues
    maybe'
      queue_
      (throwError err404{errBody = [i|Sorry, there is no queue #{qid}|]})
      ( \q -> do
          when
            (q.key /= key)
            (throwError err404{errBody = [i|You can't read messages from the queue #{qid}|]})
          msgs <- liftIO $ atomically $ flushTQueue q.queue
          curTime <- liftIO getCurrentTime
          let msgs_ = (\MessageSend{contents, sentAt} -> MessageReceive{..}) <$> msgs
          return MessagesReceive{messages = msgs_, serverSentAt = curTime}
      )
  handleRemoveQueues :: MessageRemoveQueues -> ServerM ()
  handleRemoveQueues MessageRemoveQueues{..} = do
    env <- ask
    liftIO $ atomically $ forM_ queues $ \x -> delete x env.queues

data SpawnerConfig = SpawnerConfig
  { _DELAY_CONTACT_MAX :: Double
  , _DELAY_CONTACT_MIN :: Double
  , _DELAY_SPAWNER :: Double
  , _LOG_ENABLED :: Bool
  , _RETRY_TIMES :: Int
  }
  deriving (Generic, ToJSON, FromJSON)

data MainConfig = MainConfig
  { contacts :: FilePath
  , contactsDir :: FilePath
  , server :: FilePath
  , spawner :: FilePath
  , port :: Int
  }
  deriving (Generic, FromJSON, ToJSON)

data ServerConfig = ServerConfig
  { _KEY_LENGTH :: Int
  , _LOG_ENABLED :: Bool
  , _RANDOM_SEQ_BLOCK_SIZE :: Int
  }
  deriving (Generic, FromJSON, ToJSON)

mkClientEnv_ :: MainConfig -> Log -> IO ClientEnv
mkClientEnv_ mainConfig log = do
  manager <- newManager defaultManagerSettings
  spawnerConfig :: SpawnerConfig <- decodeFileThrow mainConfig.spawner
  let _RETRY_TIMES = spawnerConfig._RETRY_TIMES
      _DELAY_CONTACT_MIN = spawnerConfig._DELAY_CONTACT_MIN
      _DELAY_CONTACT_MAX = spawnerConfig._DELAY_CONTACT_MAX
      _DELAY_SPAWNER = spawnerConfig._DELAY_SPAWNER
      _LOG_ENABLED = spawnerConfig._LOG_ENABLED
      _PORT = mainConfig.port
      _CONTACTS_CONFIG_PATH = mainConfig.contacts
  return ClientEnv{..}

mkServerEnv_ :: MainConfig -> Log -> IO ServerEnv
mkServerEnv_ mainConfig log = do
  serverConfig :: ServerConfig <- decodeFileThrow mainConfig.server
  stdGen <- initStdGen
  randomSeq <- newTVarIO $ fromJust (initRandomSeq serverConfig._RANDOM_SEQ_BLOCK_SIZE)
  let _KEY_LENGTH = serverConfig._KEY_LENGTH
      _LOG_ENABLED = serverConfig._LOG_ENABLED
  queues :: Map QueueID Queue <- newIO
  return ServerEnv{..}

main :: IO ()
main = do
  log <- newTQueueIO

  let configDir = "data/main.yaml"
  mainConfig :: MainConfig <- decodeFileThrow configDir
  -- prepare contacts dir
  doesDirectoryExist mainConfig.contactsDir >>= (`when` removeDirectoryRecursive mainConfig.contactsDir)
  clientEnv <- mkClientEnv_ mainConfig log
  serverEnv <- mkServerEnv_ mainConfig log
  let
    runServer_ = run mainConfig.port $ app serverEnv
    runSpawner_ = runReaderT' clientEnv runSpawner
    runLogger_ = runLogger log
    runAll =
      runConcurrently $
        (,,)
          <$> Concurrently runServer_
          <*> Concurrently runSpawner_
          <*> Concurrently runLogger_
  bracketOnError
    ( do
        runAll
    )
    return
    (\err -> T.putStrLn [i|Terminating app on error: #{err}|])

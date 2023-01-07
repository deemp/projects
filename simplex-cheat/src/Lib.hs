{-
# Simplex-cheat

We're going to implement a simplex-like chat app.
It will have a server and bots talking via that server.
The implementation is given below.

## Language extensions

We'll need the following language extensions and pragmas:
-}

{- FOURMOLU_DISABLE -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Control.Exception (SomeException, bracketOnError, catch)
import Control.Monad (
  forM_,
  forever,
  replicateM,
  unless,
  when,
 )
import Control.Monad.Catch (
  Exception (toException),
 )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask), ReaderT (..), asks)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Aeson.Key (fromString, toText)
import Data.Aeson.KeyMap (toList)
import Data.Binary (encode)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (UTCTime)
import Data.Time.Clock (getCurrentTime)
import Data.Tuple (swap)
import Data.Yaml.Aeson (
  FromJSON (parseJSON),
  Parser,
  ToJSON,
  Value,
  decodeFileThrow,
  encodeFile,
  withObject,
  (.:),
 )
import Fmt ((|+))
import Fmt.Internal.Core ((+|))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Capture, Get, Handler, HasServer (ServerT), JSON, Post, Proxy (..), ReqBody, ServerError (..), err404, serve, throwError, type (:<|>) (..), type (:>))
import Servant.Client (BaseUrl (BaseUrl), ClientError, ClientM, Scheme (..), client, mkClientEnv)
import Servant.Client.Internal.HttpClient (runClientM)
import Servant.Server (hoistServer)
import StmContainers.Map (Map, insert, lookup, newIO)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath (takeDirectory)
import System.Random.Shuffle (shuffle')
import System.Random.Stateful (StdGen, globalStdGen, initStdGen, uniformRM)
import TextShow (Builder, TextShow (showb, showt), fromText)
import Prelude hiding (log, lookup)

{-

## Implementation

Now, we can proceed to the implementation of the app.

There will be simplex message queues with ids

There is a config that determines
  who will request these queues: Alice or Bob
  where to write the chat config for Alice and Bob

See [contacts](./data/configs/contacts.yaml)

meaning Alice and Bob agreed on the meeting place (meeting_file)
and Alice initiates the meeting

Bob waits for "meeting_file" changes
Alice queries the server for invite
Server creates the queues and writes their ids into the "meeting_file"
{"Alice": { "id": 3, "key": "some_key" }, "Bob": {"id" : 5, "key": "some_other_key"}}

Now, Alice sends to Bob a message by making POST server/send/5 message_data
Bob can read messages from ALICE as an array of objects by making GET server/receive/5

When this functionality is ready, we can add writing messages to a database

Users threads
There should be a thread that creates users
It waits for changes in the config.yaml

if there are changes, it goes through users and checks that they exist
-}

{-
There will be a thread Spawner that handles spawning the persons

It will look at config.yaml and see if there exists Alice.Bob.meeting_file

Spawner tasks
- Read config.yaml
- For each Alice.Bob look if
  - Alice exists
    - If no, spawn Alice
  - Bob exists
    - If no, spawn Bob
  - filename exists
    - if file doesn't exist
    - write into Alice's newContacts list (contact,filename,starts = true)
  Then, Alice will send a request to the server to create such file for her and Bob
  This thread will notify Bob to watch until that file exists
  When that file exists, Bob should start sending and receiving messages
- Write a TMVar with a user's queue with pairs (contact,filename)
- Remember and update a map with user thread ids
- On each config read, actualise Alice's contacts Hashset
  - If a filename changes, notify a user (via some TVar)
  - If Alice doesn't exist, kill Alice's thread by id

- TODO: multiple spawners
-}

{-

- Perhaps just use a hashset instead of a queue, convert it to a list and traverse each time
- Spawner writes to this hashset, user reads from it
- There are records {contact,filename,Maybe starts}

The tasks of Alice and other users
- Go through contacts queue with pairs (contact,filename,starts) given by spawner
- ask server to create a file
- Move them into a local queue (contact,filename)
- Read queues ids from filename
- Receive messages and; print them
- Send messages
- Ignore contacts with non-existing file, return them to queue. These files may not yet exist
- Remove contacts that aren't in a Hashset from the local contacts queue

- TODO: Remove duplicate contacts
-}

{-
If Alice wants to start a conversation with Bob,
she will look at that file
and send its name. Alice will add it to her map of contacts
Alice shouldn't see config.yaml
-}

newtype Name = Name Text
  deriving (Generic, Show, Eq)
  deriving anyclass (FromJSON, ToJSON, Hashable)

newtype MessageMakeChat = MessageMakeChat
  { file :: FilePath
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

data MessageSend = MessageSend
  { contents :: Text
  , sentAt :: UTCTime
  , key :: SecretKey
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

data MessageReceive = MessageReceive
  { contents :: Text
  , sentAt :: UTCTime
  }
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

type QueueID = Int

type MakeChat =
  "make"
    :> ReqBody '[JSON] MessageMakeChat
    :> Post '[JSON] ()

type SendMessage =
  "send"
    :> Capture "id" QueueID
    :> ReqBody '[JSON] MessageSend
    :> Post '[JSON] ()

data MessagesReceive = MessagesReceive
  { -- when server sent message to a receiver
    serverSentAt :: UTCTime
  , messages :: [MessageReceive]
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON)

type ReceiveMessages =
  "receive"
    :> Capture "id" QueueID
    :> Capture "key" SecretKey
    :> Get '[JSON] MessagesReceive

type SecretKey = Text

api :: Proxy Api
api = Proxy

type Api =
  "server"
    :> SendMessage
    :<|> ReceiveMessages
    :<|> MakeChat

sendMessage :: QueueID -> MessageSend -> ClientM ()
receiveMessage :: QueueID -> SecretKey -> ClientM MessagesReceive
makeChat :: MessageMakeChat -> ClientM ()
sendMessage :<|> receiveMessage :<|> makeChat = client api

data SpawnerConfig = SpawnerConfig
  { _DELAY_CONTACT_MAX :: Double
  , _DELAY_CONTACT_MIN :: Double
  , _DELAY_SPAWNER :: Double
  , _LOG_ENABLED :: Bool
  , _RETRY_TIMES :: Int
  }
  deriving (Generic, ToJSON, FromJSON)

mkEnv :: MainConfig -> Log -> IO ClientEnv
mkEnv mainConfig log = do
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

mkServerEnv :: MainConfig -> Log -> IO ServerEnv
mkServerEnv mainConfig log = do
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
  removeDirectoryRecursive mainConfig.contactsDir `catch` (\(_ :: SomeException) -> return ())
  clientEnv <- mkEnv mainConfig log
  serverEnv <- mkServerEnv mainConfig log
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
    (\err -> T.putStrLn $ "Terminating app on error: " <> showt err)

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

runSpawner :: App ClientEnv ()
runSpawner = do
  config <- liftIO $ newTVarIO HM.empty
  users <- liftIO $ newTVarIO (HM.empty :: HM.HashMap Name User)
  let spawnerB = showb (Name "Spawner")
  _DELAY_SPAWNER <- asks (_DELAY_SPAWNER :: ClientEnv -> Double)

  forever do
    liftIO $ sleepRandom _DELAY_SPAWNER (_DELAY_SPAWNER + 1)
    res :: Either SomeException () <- runExceptT do
      configPath <- asks _CONTACTS_CONFIG_PATH
      configOld <- liftIO $ readTVarIO config
      configNew <- unContactsConfig <$> decodeFileThrow configPath
      liftIO $ atomically $ writeTVar config configNew
      let
        diffOld = HM.toList $ HM.difference configOld configNew
        diffNew = HM.toList $ HM.difference configNew configOld
      users' <- liftIO $ readTVarIO users
      liftIO $
        forM_
          diffOld
          ( \((name1, _), ConfigValue{..}) -> do
              maybe
                (return ())
                ( \User{..} -> do
                    throwTo threadId NotInConfigError
                    doesFileExist file >>= (`when` removeFile file)
                )
                (HM.lookup name1 users')
          )
      forM_
        diffNew
        ( \((name1, name2), ConfigValue{..}) -> do
            _RETRY_TIMES <- asks (_RETRY_TIMES :: ClientEnv -> Int)
            let sendContact queue =
                  atomically $
                    writeTQueue
                      queue
                      Contact
                        { name = name2
                        , file
                        , starts = if name1 == starts then Self else Another
                        , retryTimes = _RETRY_TIMES
                        , messageNumber = 0
                        }
            maybe'
              (HM.lookup name1 users')
              ( -- if a user doesn't exist
                do
                  -- spawn a user
                  newQueue <- liftIO newTQueueIO
                  env <- ask
                  writeLog $ spawnerB |+ "created a user: " +| showb name1
                  threadId_ <- liftIO $ forkIO $ runReaderT' env (runUser name1 newQueue)

                  -- record new user
                  liftIO $
                    atomically $
                      modifyTVar'
                        users
                        ( HM.insert name1 User{threadId = threadId_, queue = newQueue}
                        )

                  -- send the contact to that user
                  liftIO $ sendContact newQueue
              )
              ( -- if a user exists
                \User{..} -> do
                  -- as it's a new part of config
                  -- either the contact or the file should have been updated
                  liftIO $ sendContact queue
              )
        )
    either'
      res
      (\x -> writeLog $ spawnerB |+ "error: " +| showb x)
      return

instance TextShow Name where
  showb :: Name -> Builder
  showb (Name name) = " [ " +| fromText name |+ " ] "

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

instance TextShow ClientError where
  showb :: ClientError -> Builder
  showb err = showb (toException err)

newtype ContactError = ContactUnavailable {name :: Name}
  deriving (Show, Exception)

instance TextShow ContactError where
  showb :: ContactError -> Builder
  showb ContactUnavailable{..} = "Contact" +| showb name |+ "is unavailable"

runUser :: Name -> TQueue Contact -> App ClientEnv ()
runUser selfName queue = do
  manager' <- asks manager
  _DELAY_CONTACT_MIN <- asks (_DELAY_CONTACT_MIN :: ClientEnv -> Double)
  _DELAY_CONTACT_MAX <- asks (_DELAY_CONTACT_MAX :: ClientEnv -> Double)
  let baseUrl_ = BaseUrl Http "localhost" 8080 ""
      req clientM = runClientM clientM (mkClientEnv manager' baseUrl_)
      selfB :: Builder
      selfB = showb selfName
      delayContact = liftIO $ sleepRandom _DELAY_CONTACT_MIN _DELAY_CONTACT_MAX
  forever do
    delayContact
    res :: Either SomeException () <- runExceptT do
      contact <- liftIO $ atomically $ readTQueue queue
      let anotherB :: Builder
          anotherB = showb contact.name
      existsFile <- liftIO $ doesFileExist contact.file
      unless existsFile do
        if contact.retryTimes > 0
          then do
            case contact.starts of
              Self -> do
                res <- liftIO $ req (makeChat MessageMakeChat{file = contact.file})
                either
                  ( \err ->
                      writeLog $
                        (selfB |+ "error when requesting a connection with ")
                          +| (anotherB |+ ": " +| showb err)
                  )
                  (\_ -> writeLog $ selfB |+ "sent a request to connect with" +| anotherB)
                  res
              Another -> do
                writeLog $ selfB |+ "waits until" +| anotherB |+ "sends a request to connect"
            -- TODO decide where to write updated contact
            let updatedContact = contact{retryTimes = contact.retryTimes - 1}
            liftIO $ atomically $ writeTQueue queue updatedContact
            throwE $ toException (ContactUnavailable contact.name)
          else writeLog $ selfB |+ "was unable to connect with" +| anotherB |+ ". No attempts left"
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
                  { contents = selfB |+ " : " +| showb contact.messageNumber
                  , sentAt = curTime
                  , key = config.key
                  }
            )
      either'
        sendMessageRes
        ( \err -> writeLog $ selfB |+ "was unable to send a message to" +| anotherB |+ ": " +| showb err
        )
        ( \_ -> do
            writeLog $ selfB |+ "sent a message #" +| showb contact.messageNumber |+ " to" +| anotherB
            liftIO $ atomically $ writeTQueue queue contact{messageNumber = contact.messageNumber + 1}
        )
      receiveMessagesRes <- liftIO $ req (receiveMessage receiveFrom config.key)
      either'
        receiveMessagesRes
        ( \err -> writeLog $ selfB |+ "was unable to receive messages from" +| anotherB |+ ": " +| showb err
        )
        ( \messages -> do
            let messages_ :: [Text] = (\x -> x.contents) <$> messages.messages
            writeLogConsecutive ((selfB |+ "received messages from" +| anotherB |+ ":") : messages_)
        )
    either'
      res
      ( \err -> writeLog $ selfB |+ "got an error when processing the contacts queue: " +| showb err
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

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' val x y = maybe x y val

nt :: ServerEnv -> ServerM a -> Handler a
nt s x = runReaderT x s

app :: ServerEnv -> Application
app s = serve api $ hoistServer api (nt s) runServer

runServer :: ServerT Api ServerM
runServer = handleSendMessage :<|> handleReceiveMessage :<|> handleMakeChat
 where
  serverB = " [ Server ] " :: Builder
  handleMakeChat :: MessageMakeChat -> ServerM ()
  handleMakeChat MessageMakeChat{..} = do
    let dir = takeDirectory file
    liftIO $ createDirectoryIfMissing True dir
    gen <- asks stdGen
    randomSeq_ <- liftIO =<< asks (readTVarIO . randomSeq)
    let (id1, randomSeq1) = nextRandomNumber gen randomSeq_
        (id2, randomSeq2) = nextRandomNumber gen randomSeq1
    randomSeq' <- asks randomSeq
    liftIO $ atomically $ modifyTVar randomSeq' (const randomSeq2)
    len <- asks (_KEY_LENGTH :: ServerEnv -> Int)
    key_ <- T.pack <$> replicateM len (uniformRM ('a', 'z') globalStdGen)
    let chatConfig = ChatConfig{starts = id1, another = id2, key = key_}
    liftIO $ encodeFile file chatConfig
    queues_ <- asks queues
    liftIO $ atomically do
      queue1 <- newTQueue
      insert Queue{queue = queue1, key = key_} id1 queues_
      queue2 <- newTQueue
      insert Queue{queue = queue2, key = key_} id2 queues_
    writeLog $ serverB |+ "created queues: " +| showb id1 |+ " and " +| showb id2
  handleSendMessage :: QueueID -> MessageSend -> ServerM ()
  handleSendMessage qid msg = do
    queuesMap <- asks queues
    queue_ <- liftIO $ atomically $ lookup qid queuesMap
    maybe'
      queue_
      (throwError err404{errBody = "Sorry, there is no queue with id" <> encode qid})
      ( \q@Queue{..} -> do
          when
            (q.key /= msg.key)
            (throwError err404{errBody = "You can't send messages to the queue " <> encode qid})
          liftIO $ atomically $ writeTQueue queue msg
      )
  handleReceiveMessage :: QueueID -> SecretKey -> ServerM MessagesReceive
  handleReceiveMessage qid key = do
    queuesMap <- asks queues
    queue_ <- liftIO $ atomically $ lookup qid queuesMap
    maybe'
      queue_
      (throwError err404{errBody = "Sorry, there is no queue with id" <> encode qid})
      ( \q -> do
          when
            (q.key /= key)
            (throwError err404{errBody = "You can't read messages from the queue " <> encode qid})
          msgs <- liftIO $ atomically $ flushTQueue q.queue
          curTime <- liftIO getCurrentTime
          let msgs_ = (\MessageSend{contents, sentAt} -> MessageReceive{..}) <$> msgs
          return MessagesReceive{messages = msgs_, serverSentAt = curTime}
      )
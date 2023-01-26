{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TryEffectful.Dynamic where

import Control.Exception (IOException)
import Control.Monad (replicateM)
import Control.Monad.Catch (catch)
import Control.Monad.Cont (MonadIO (..))
import Data.Char (chr)
import Data.Map.Strict qualified as M
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, runEff, runPureEff, type (:>))
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, localSeqUnlift, localSeqUnliftIO, reinterpret, send)
import Effectful.Error.Dynamic
import Effectful.State.Static.Local
import GHC.Clock (getMonotonicTime)
import System.IO qualified as IO
import Prelude hiding (readFile)

data FileSystem :: Effect where
  ReadFile :: FilePath -> FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

type instance DispatchOf FileSystem = Dynamic

readFile :: (HasCallStack, FileSystem :> es) => FilePath -> Eff es String
readFile path = send (ReadFile path)

writeFile :: (HasCallStack, FileSystem :> es) => FilePath -> String -> Eff es ()
writeFile path content = send (WriteFile path content)

newtype FsError = FsError String deriving (Show)

runFileSystemIO :: (IOE :> es, Error FsError :> es) => Eff (FileSystem : es) a -> Eff es a
runFileSystemIO = interpret $ \_ -> \case
  ReadFile path -> adapt $ IO.readFile path
  WriteFile path contents -> adapt $ IO.writeFile path contents
 where
  adapt m = liftIO m `catch` \(e :: IOException) -> throwError . FsError $ show e

runFileSystemPure :: Error FsError :> es => M.Map FilePath String -> Eff (FileSystem : es) a -> Eff es a
runFileSystemPure fs0 = reinterpret (evalState fs0) $ \_ -> \case
  ReadFile path ->
    gets (M.lookup path) >>= \case
      Just contents -> pure contents
      Nothing -> throwError . FsError $ "File not found: " ++ show path
  WriteFile path contents -> modify $ M.insert path contents

action :: (FileSystem :> es) => Eff es Bool
action = do
  file <- readFile "nix-managed.cabal"
  pure $ not (null file)

-- >>>:t action
-- action :: (FileSystem :> es) => Eff es Bool

-- >>>runEff . runError @FsError . runFileSystemIO $ action
-- Right True

-- >>>runPureEff . runErrorNoCallStack @FsError . runFileSystemPure M.empty $ action
-- Left (FsError "File not found: \"nix-managed.cabal\"")

data Profiling :: Effect where
  Profile :: String -> m a -> Profiling m a

type instance DispatchOf Profiling = Dynamic

profile :: (HasCallStack, Profiling :> es) => String -> Eff es a -> Eff es a
profile label action = send (Profile label action)

runProfiling :: IOE :> es => Eff (Profiling : es) a -> Eff es a
runProfiling = interpret $ \env -> \case
  Profile label action -> localSeqUnliftIO env $ \unlift -> do
    t1 <- getMonotonicTime
    r <- unlift action
    t2 <- getMonotonicTime
    putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1) ++ " seconds."
    pure r

runNoProfiling :: Eff (Profiling : es) a -> Eff es a
runNoProfiling = interpret $ \env -> \case
  Profile label action -> localSeqUnlift env $ \unlift -> unlift action

action1 :: (Profiling :> es, IOE :> es) => Eff es ()
action1 = profile "greet" . liftIO $ putStrLn "Hello!"

testProfiling :: IO ()
testProfiling = runEff . runProfiling $ action1

testNoProfiling :: IO ()
testNoProfiling = runEff . runNoProfiling $ action1

class Monad m => MonadRNG m where
  randomInt :: m Int

randomString :: MonadRNG m => Int -> m String
randomString n = map chr <$> replicateM n randomInt

data RNG :: Effect where
  RandomInt :: RNG m Int

type instance DispatchOf RNG = Dynamic

instance RNG :> es => MonadRNG (Eff es) where
  randomInt :: (RNG :> es) => Eff es Int
  randomInt = send RandomInt

runDummyRNG :: Eff (RNG : es) a -> Eff es a
runDummyRNG = interpret $ \_ -> \case
  RandomInt -> pure 55

testDummy :: IO String
testDummy = runEff . runDummyRNG $ randomString 3

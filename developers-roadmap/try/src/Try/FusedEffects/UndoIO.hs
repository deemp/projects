{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Try.FusedEffects.UndoIO (
  main1,
  main,
) where

import Control.Algebra (Algebra (alg), send, type (:+:) (L, R))
import Control.Applicative (Alternative)
import Control.Carrier.Error.Either (Catch, Has, Throw, catchError, runError, throwError)
import Control.Carrier.Fail.Either qualified as Fail
import Control.Carrier.Lift (sendM)
import Control.Carrier.State.Strict (StateC (..), runState)
import Control.Effect.Exception (Lift, bracket, try)
import Control.Exception.Lifted qualified as CL (bracket, catch, try)
import Control.Monad (MonadPlus, replicateM_)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Catch qualified as CE
import Control.Monad.Except (runExceptT)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (MonadTrans (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Writer (WriterT (..))
import Control.Monad.Trans.Writer qualified as W (WriterT)
import Control.Monad.Writer.Class qualified as WC (MonadWriter, tell)
import Data.Data (Typeable)
import Data.Foldable (sequenceA_)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)

data WriterStack w (m :: Type -> Type) k where
  Tell :: w -> WriterStack w m ()
  Listen :: m a -> WriterStack w m (w, a)
  Censor :: (w -> w) -> m a -> WriterStack w m a

tell :: forall w m sig. Has (WriterStack w) sig m => w -> m ()
tell = send . Tell

listen :: Has (WriterStack w) sig m => m a -> m (w, a)
listen = send . Listen

censor :: Has (WriterStack w) sig m => (w -> w) -> m a -> m a
censor f = send . Censor f

newtype WriterStackC w m a = WriterStackC {runWriterStackC :: StateC w m a}
  deriving (Alternative, Applicative, Functor, Monad, Fail.MonadFail, MonadFix, MonadIO, MonadPlus, MonadTrans)

runWriterStack :: forall w a m. Monoid w => WriterStackC w m a -> m (w, a)
runWriterStack (WriterStackC m) = runState mempty m

execWriterStack :: forall w a m. (Monoid w, Functor m) => WriterStackC w m a -> m w
execWriterStack = fmap fst . runWriterStack

instance (Monoid w, Algebra sig m) => Algebra (WriterStack w :+: sig) (WriterStackC w m) where
  alg hdl sig ctx =
    -- this is just to observe the type of hdl
    let hdl1 = hdl
     in WriterStackC $ case sig of
          L writer -> StateC $ \w -> case writer of
            Tell w1 -> do
              let !w2 = w1 <> w
              return (w2, ctx)
            Listen m -> do
              (w1, a) <- runWriterStack (hdl (m <$ ctx))
              let !w2 = w1 <> w
              return (w2, (w1,) <$> a)
            Censor f m -> do
              (w1, a) <- runWriterStack (hdl (m <$ ctx))
              let !w2 = f w1 <> w
              return (w2, a)
          R other -> alg (runWriterStackC . hdl) (R other) ctx

tell1 :: (Has (WriterStack [a]) sig m) => a -> m ()
tell1 a = tell [a]

l :: [Int]
l = (4 :: Int) <$ ([3, 2, 5] :: [Int]) $> 5

-- >>> l
-- [5,5,5]

type Log = [Int]

sl2 :: Log
sl2 = take 5 $
  (runIdentity . execWriterStack) do
    replicateM_ 1000000 (let !a = tell ([3] :: [Int]) in a)

-- >>> s2
-- [3,3,3,3,3]

someActions :: forall m sig. (MonadIO m, Has (WriterStack [Int]) sig m, Has (WriterStack [IO ()]) sig m) => m ()
someActions = do
  tell @[Int] [1, 2]
  tell @[IO ()] [putStr "world!\n"]
  tell @[IO ()] [putStr "Hello, "]

main1 :: IO ()
main1 = do
  s <- (fst <$>) $ runWriterStack @[IO ()] . runWriterStack @[Int] @() $ someActions
  sequenceA_ s

main :: IO ()
main = print "Hello, world!"

{-
## Writer and exceptions

### Using `Lift IO`
-}

data FileError = WriteError FilePath | ReadError FilePath deriving (Typeable, Show)

instance CE.Exception FileError

someActions1 :: forall m sig. (Has (Lift IO) sig m, Has (Throw FileError) sig m, Has (Catch FileError) sig m, Has (WriterStack [IO ()]) sig m) => m ()
someActions1 = flip catchError (\(_ :: FileError) -> tell @[IO ()] [putStr "Not hello, "]) do
  tell @[IO ()] [putStr "world!\n"]
  _ :: Either CE.SomeException () <-
    try $
      bracket
        (sendM $ readFile "SomeFile")
        (\_ -> tell @[IO ()] [print "File is here!"])
        (\_ -> tell @[IO ()] [print "File is not here!"])
  _ <- throwError $ ReadError "No such file"
  tell @[IO ()] [putStr "Hello, "]

main2 :: IO ()
main2 = do
  s <- (fst <$>) $ runWriterStack @[IO ()] . runError @FileError $ someActions1
  sequenceA_ s

main3 :: IO ()
main3 = do
  s <- runError @FileError . runWriterStack @[IO ()] $ someActions1
  case s of
    Left x -> print x
    Right (x, _) -> sequenceA_ x

{-
## Trying `mtl` stack

Still, Writer's log doesn't go into handlers
-}

newtype MyStack e w a = MyStack {runMyStack :: ExceptT e (W.WriterT w IO) a}
  deriving (Functor, Applicative, Monad, WC.MonadWriter w, MonadIO, MonadMask, MonadCatch, MonadThrow)

someActions2 :: MyStack FileError [IO ()] ()
someActions2 = flip CE.catch (\(_ :: FileError) -> WC.tell [putStr "Not hello, "]) do
  WC.tell [putStr "world!\n"]
  _ :: Either CE.SomeException () <-
    CE.try $
      CE.bracket
        (liftIO $ readFile "SomeFile")
        (\_ -> WC.tell [putStr "File is here!"])
        (\_ -> WC.tell [putStr "File is not here!"])
  _ <- CE.throwM $ ReadError "No such file"
  WC.tell [putStr "Hello, "]
  return ()

main4 :: IO ()
main4 = do
  s <- (snd <$>) $ runWriterT . runExceptT . runMyStack $ someActions2
  sequenceA_ s

{-
### Trying lifted-base
-}

someActions3 :: forall m sig. (MonadBaseControl IO m, Has (Lift IO) sig m, Has (Throw FileError) sig m, Has (WriterStack [IO ()]) sig m) => m ()
someActions3 = flip CL.catch (\(_ :: FileError) -> tell @[IO ()] [putStr "Not hello, "]) do
  tell @[IO ()] [putStr "world!\n"]
  _ :: Either CE.SomeException () <-
    CL.try $
      CL.bracket
        (sendM $ readFile "SomeFile")
        (\_ -> tell @[IO ()] [print "File is here!"])
        (\_ -> tell @[IO ()] [print "File is not here!"])
  _ <- throwError $ ReadError "No such file"
  tell @[IO ()] [putStr "Hello, "]

-- main5 :: IO ()
-- main5 = do
--   s <- (fst <$>) $ runWriterStack @[IO ()] . runError @FileError $ someActions3
--   sequenceA_ s

increment :: Int -> Int
increment x = x + 1

wrappedInt :: Maybe Int
wrappedInt = Just 3

wrappedIncrement :: Maybe (Int -> Int)
wrappedIncrement = Just increment

s1 :: (Int -> Int) -> (Maybe Int -> Maybe Int)
s1 = fmap

s1' :: (Int -> Int) -> (Maybe Int -> Maybe Int)
s1' = undefined

s2 :: Maybe (Int -> Int) -> (Maybe Int -> Maybe Int)
s2 = (<*>)

s2' :: Maybe (Int -> Int) -> (Maybe Int -> Maybe Int)
s2' = undefined

wrappingIncrement :: Int -> Maybe Int
wrappingIncrement x = Just (increment x)

s3 :: Int -> Maybe Int
s3 = pure

s3' :: Int -> Maybe Int
s3' = undefined

s4 :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
s4 = (>>=)

s4' :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
s4' = undefined
```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Try.Monads.FunctionalDependencies where

import Data.Functor ((<&>))

-- Fundeps exercise - https://www.fpcomplete.com/haskell/tutorial/fundeps/#exercises

newtype PersonReader a = PersonReader {runPersonReader :: Person -> a}
  deriving (Functor, Applicative, Monad)

class Monad m => MonadReader env m | m -> env where
  ask :: m env

data Person = Person
  { nameP :: String
  , ageP :: Int
  }
  deriving (Show)

askAge :: MonadReader Person m => m Int
askAge = ask <&> ageP

askName :: MonadReader Person m => m String
askName = ask <&> nameP

greeting :: forall m. (Monad m, MonadReader Person m) => m String
greeting = do
  name <- askName
  age <- askAge
  pure $ name ++ " is " ++ show age ++ " years old"

instance MonadReader Person PersonReader where
  ask :: PersonReader Person
  ask = PersonReader id

greetingId :: String
greetingId = runPersonReader (greeting @PersonReader) Person{nameP = "ah", ageP = 3}

-- >>>greetingId
-- "ah is 3 years old"
```

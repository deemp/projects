{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Try.Lens.MissingKey where

import Control.Lens
import Data.Map
import qualified Data.Map as Map
import GHC.Generics (Generic)

mp1 :: Map.Map String (Map.Map String Int)
mp1 = Map.fromList [("a", Map.fromList [("c", 3), ("d", 5)])]

t = mp1 ^? ix "a" . ix "d"

-- >>> t
-- Just 5

class Ixed m => PathIxed m where
  pat :: Index m -> Prism' ([Index m], Maybe m) (([Index m], Maybe (IxValue m)), m)
  pat' :: (PathIxed m, Applicative f, s ~ ([Index m], Maybe (IxValue m)), a ~ ([Index m], Maybe m)) => Index m -> (s -> f s) -> (a -> f a)
  pat' x = pat x . _1

instance Ord a => PathIxed (Map a b) where
  pat p = prism embed match
   where
    embed :: (([a], Maybe b), Map a b) -> ([a], Maybe (Map a b))
    embed ((path, v), parent) = (path <> [p], v >>= \v' -> pure (Map.insert p v' parent))

    match :: ([a], Maybe (Map a b)) -> Either ([a], Maybe (Map a b)) (([a], Maybe b), Map a b)
    match (path, m) =
      case m of
        Nothing -> Left (path <> [p], Nothing)
        Just m' ->
          case Map.lookup p m' of
            Nothing -> Left (path <> [p], Nothing)
            Just p' -> Right ((path <> [p], Just p'), m')

t1 = ([], Just mp1) ^? pat' "a" . pat' "c"

-- >>> t1
-- Just (["a","c"],Just 3)

t2 = matching' (pat' "c") ([], Just mp1)

-- >>> t2
-- Left (["c"],Nothing)

t3 = matching' (pat' "c" . pat' "d") ([], Just mp1)

-- >>> t3
-- Left (["c"],Nothing)

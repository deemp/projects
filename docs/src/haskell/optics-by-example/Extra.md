```haskell
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Extra where

import Control.Lens
import Control.Monad.State (execState, modify)
import Data.Generics.Labels ()
import Data.Map (fromList)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Traversable (for)
```

## `non`

```haskell
ex1 :: Map.Map Text.Text Int
ex1 = fromList [("WORLD", 456)]

ex2 :: Map.Map Text.Text Int
ex2 = ex1 & at "HELLO" . non 678 .~ 3

-- >>> x1
-- fromList [("HELLO",3),("WORLD",456)]
```

## update at multiple indices

```haskell
ex3 :: [Text.Text] -> Int -> Map.Map Text.Text Int -> Map.Map Text.Text Int
ex3 ks val = execState (traverse (\k -> modify (at k ?~ val)) ks)

ex4 :: Map.Map Text.Text Int
ex4 = Map.empty & ex3 ["a", "b", "c"] 4

-- >>> ex4
-- fromList [("a",4),("b",4),("c",4)]

ex5 :: Map.Map String Integer
ex5 = Map.empty &~ for ["b", "c", "d"] (\k -> at k ?= 10)

-- >>> ex5
-- fromList [("b",10),("c",10),("d",10)]
```

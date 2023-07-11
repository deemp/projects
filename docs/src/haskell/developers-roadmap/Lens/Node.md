```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Try.Lens.Node where

import Control.Lens (Iso, Iso', Plated, cosmos, filtered, has, ifiltered, indexing, iso, only, reversed, taking, traverseOf, traversed, (&), (<&>), (<.), (^.), (^..), (^@..))
import Control.Lens.Plated (Plated (plate))
import Data.Data (Data)
import Data.Generics.Labels ()
import GHC.Generics (Generic)

data Node f a = Node
  { nodeData :: a
  , nodeName :: String
  , nodeChildren :: [Node f a]
  }
  deriving stock (Data, Generic)

tree :: Node f Int
tree =
  Node
    { nodeData = 3
    , nodeName = "1"
    , nodeChildren =
        [ Node
            { nodeData = 4
            , nodeName = "2"
            , nodeChildren =
                [ Node{nodeData = 5, nodeName = "1", nodeChildren = []}
                ]
            }
        , Node
            { nodeData = 6
            , nodeName = "1"
            , nodeChildren =
                [ Node
                    { nodeData = 7
                    , nodeName = "2"
                    , nodeChildren =
                        [ Node{nodeData = 8, nodeName = "3", nodeChildren = []}
                        ]
                    }
                ]
            }
        ]
    }

changeTraversalOrderTo :: forall t2 t1 a. Iso' (Node t1 a) (Node t2 a)
changeTraversalOrderTo = iso change change
 where
  change Node{..} = Node{nodeChildren = nodeChildren ^.. traversed . changeTraversalOrderTo, ..}

ex :: forall f. Plated (Node f Int) => Node f Int -> [(Int, Int)]
ex tree = tree ^@.. changeTraversalOrderTo @f . indexing cosmos <. filtered (has (#nodeName . only "1")) . #nodeData

data InOrder

instance Plated (Node InOrder a) where
  plate f Node{..} = do
    nodeChildren <- traverse f nodeChildren
    pure Node{..}

-- >>> ex @InOrder tree
-- [(0,3),(2,5),(3,6)]

data WeirdOrder

instance (Num a, Ord a) => Plated (Node WeirdOrder a) where
  plate f Node{..} = do
    nodeChildren <- traverseOf (\a b -> (<>) <$> (traversed . ifiltered (\i v -> odd i && nodeData <= 4)) a b <*> traversed a b) f nodeChildren
    pure $ Node{..}

-- >>> ex @WeirdOrder tree
-- [(0,3),(1,6),(5,5),(6,6)]
```

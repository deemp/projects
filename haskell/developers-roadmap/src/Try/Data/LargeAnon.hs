{-
# large-anon: Practical scalable anonymous records for Haskell

[source](https://well-typed.com/blog/2022/04/large-anon/)

TODO: example with generic lens ([issue](https://github.com/well-typed/large-records/issues/150))
-}

{- FOURMOLU_DISABLE -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Try.Data.LargeAnon where

import Data.Record.Anon
import Data.Record.Anon.Overloading
import Data.Record.Anon.Simple

magenta :: Record ["red" := Double, "green" := Double, "blue" := Double]
magenta = ANON{red = 1, green = 0, blue = 1}

purple :: Record '["red" ':= Double, "green" ':= Integer, "blue" ':= Double]
purple = insert #red 0.5 $ insert #green 0 $ insert #blue 0.5 empty

b :: Double
b = purple.blue

-- >>> b
-- 0.5

reduceRed :: (RowHasField "red" r Double) => Record r -> Record r
reduceRed c = c{red = c.red * 0.9}

ex1 :: Record '["red" ':= Double, "green" ':= Double, "blue" ':= Double]
ex1 = reduceRed magenta


-- TODO how to show?

-- >>> ex1
-- No instance for (AllFields
--                    '[ "red" ':= Double, "green" ':= Double, "blue" ':= Double] Show)
--   arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_a1SAw

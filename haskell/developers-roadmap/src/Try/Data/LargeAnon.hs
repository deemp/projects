{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin #-}

module Foo where

import Data.Record.Anon
import Data.Record.Anon.Overloading
import Data.Record.Anon.Simple

magenta :: Record ["red" := Double, "green" := Double, "blue" := Double]
magenta = ANON{red = 1, green = 0, blue = 1}

purple :: Record ["red" := Double, "green" := Double, "blue" := Double]
purple =
  insert #red 0.5
    $ insert #green 0
    $ insert #blue 0.5 empty

reduceRed :: (RowHasField "red" r Double) => Record r -> Record r
reduceRed c = c{red = c.red * 0.9}

ex1 = reduceRed magenta

-- >>> ex1
-- No instance for (AllFields
--                    '[ "red" ':= Double, "green" ':= Double, "blue" ':= Double] Show)
--   arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_ah5K

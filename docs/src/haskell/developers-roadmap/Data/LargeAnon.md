```haskell
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

import Control.Lens ((&), (*~))
import Data.Generics.Labels ()
import Data.Record.Anon
import Data.Record.Anon.Overloading
import Data.Record.Anon.Simple

-- magenta :: Record ["red" := Double, "green" := Double, "blue" := Double]
magenta :: Record '[ "red" ':= Integer, "green" ':= Integer, "blue" ':= Integer]
magenta = ANON{red = 1, green = 0, blue = 1}

-- purple :: Record '["red" ':= Double, "green" ':= Integer, "blue" ':= Double]
purple = insert #red 0.5 $ insert #green 0 $ insert #blue 0.5 empty

-- b :: Double
b = purple.blue

-- >>> b
-- 0.5

-- type RGB = Record ["red" := Double, "green" := Double, "blue" := Double]

-- reduceRed ::   

-- reduceRed :: (Data.Generics.Product.Fields.HasField "red" s t a a, Fractional a) => s -> t
-- reduceRed c = c & #red *~ 0.9

-- ex1 :: Record '["red" ':= Double, "green" ':= Double, "blue" ':= Double]
-- ex1 = reduceRed magenta
-- ex1 = reduceRed magenta

-- >>> ex1
-- No instance for (AllFields
--                    '[ "red" ':= Double, "green" ':= Double, "blue" ':= Double] Show)
--   arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_ah5K
```

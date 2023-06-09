{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Try.TemplateHaskell.Use where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Try.TemplateHaskell.Declare

p = $(runAdd2Typed) 2

-- >>> p
-- 4

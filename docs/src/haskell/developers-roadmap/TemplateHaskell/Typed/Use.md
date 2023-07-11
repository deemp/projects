```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Try.TemplateHaskell.Typed.Use where

import Try.TemplateHaskell.Typed.Declare (runAdd2Typed)

p = $(runAdd2Typed) 2

-- >>> p
-- 4
```

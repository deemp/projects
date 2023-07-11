```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Try.TemplateHaskell.ConstructorTags.Use where

import Try.TemplateHaskell.ConstructorTags.Declare (HydraEvent (..), deriveMapping, deriveTags)

$(deriveTags ''HydraEvent "Kind" [''Show, ''Eq])
$(deriveMapping ''HydraEvent "Kind" "getKind")
```

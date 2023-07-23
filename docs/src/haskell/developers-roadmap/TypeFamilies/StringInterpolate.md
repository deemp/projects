```haskell
{-# LANGUAGE QuasiQuotes #-}

module Try.TypeFamilies.StringInterpolate where

import Data.ByteString
import Data.String.Interpolate (i)
import Data.Text
```

# string-interpolate

Implementation [explanation](https://williamyaoh.com/posts/2019-05-27-string-interpolation-and-overlapping-instances.html) feat. Type families, Tagged Classes

```haskell
b :: Integer
b = 3

d :: Text
d = [i|comm|]

s :: ByteString
s = [i|A #{b} -c #{d}|]

-- >>> s
-- "A 3 -c comm"
```

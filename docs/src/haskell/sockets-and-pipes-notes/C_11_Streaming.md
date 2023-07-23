# 11. Streaming

```haskell
module C_11_Streaming () where

import C_01_Handles (getDataDir)
import C_03_Bytes (binaryFileResource)
import C_06_HTTP_types (MessageBody (MessageBody), Response (..))
import C_08_Responding (ok, sendResponse, status)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Lazy qualified as LBS
import GHC.IO.Handle (Handle)
import GHC.IO.IOMode (IOMode (..))
import Network.Simple.TCP (HostPreference (..), serve)
import System.FilePath ((</>))
import Text.Blaze.Html5 as Html ()

hContentsResponse :: Handle -> IO Response
hContentsResponse h = do
  fileContent <- liftIO (LBS.hGetContents h)
  let body = Just (MessageBody fileContent)
  return (Response (status ok) [] body)

fileStrict :: IO b
fileStrict = do
  dir <- getDataDir
  serve @IO HostAny "8000" \(s, _) -> runResourceT @IO do
    (_, h) <-
      binaryFileResource (dir </> "stream.txt") ReadMode
    r <- liftIO (hContentsResponse h)
    liftIO (sendResponse s r)
```

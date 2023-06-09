module Try.Concurrency.Async where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)

sec :: Int
sec = 1000000

main :: IO ()
main = withAsync (threadDelay (3 * sec) >> print "ping") (\a -> wait a >> print "pong")
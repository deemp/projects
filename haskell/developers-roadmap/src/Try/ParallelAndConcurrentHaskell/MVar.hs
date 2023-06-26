{-# LANGUAGE ScopedTypeVariables #-}

module Try.ParallelAndConcurrentHaskell.MVar where

import Control.Concurrent ()
import Control.Exception (SomeException (SomeException), catch, mask, throw)
import qualified Data.Map as Map
import UnliftIO (MVar, atomically, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, newTMVarIO, putMVar, takeMVar, takeTMVar)

{-
## MVar

- [Pessimistic and optimistic locking](https://stackoverflow.com/questions/129329/optimistic-vs-pessimistic-locking)
- [Базовая работа с MVar](https://ruhaskell.org/posts/theory/2015/02/13/mvars.html)

### MVar as a Container for Shared State

`MVar`s are lazy.

-}
name :: String
name = "name"

number :: String
number = "number"

book :: Map.Map String String
book = Map.empty

p1 :: IO ()
p1 = do
  m <- newMVar book
  putMVar m (Map.insert name number book)

{-

This places in the `MVar` the unevaluated expression `Map.insert name number book`.

Benefit: can unlock state and dont wait for `insert` to complete.
Drawback: consecutive inserts may create thunks

Solution - evaluate to WHNF

-}

p2 :: IO ()
p2 = do
  m <- newMVar book
  putMVar m $! Map.insert name number book

{-

### Fairness

> No thread can be blocked indefinitely on an MVar unless another thread holds that MVar
indefinitely.

In other words, if a thread T is blocked in takeMVar and there are regular putMVar
operations on the same MVar, it is guaranteed that at some point thread T’s takeMVar
will return. In GHC, this guarantee is implemented by keeping blocked threads in a
FIFO queue attached to the MVar, so eventually every thread in the queue will get to
complete its operation as long as there are other threads performing regular putMVar
operations (an equivalent guarantee applies to threads blocked in putMVar when there
are regular takeMVars). Note that it is not enough to merely wake up the blocked thread
because another thread might run first and take (respectively put) the MVar, causing the
newly woken thread to go to the back of the queue again, which would invalidate the
fairness guarantee. The implementation must therefore wake up the blocked thread and
perform the blocked operation in a single atomic step, which is exactly what GHC does.

A consequence of the fairness implementation is that, when multiple threads are blocked
in takeMVar and another thread does a putMVar, only one of the blocked threads becomes
unblocked. This “single wakeup” property is a particularly important performance char‐
acteristic when a large number of threads are contending for a single MVar. As we shall
see later, it is the fairness guarantee—together with the single wakeup property—that
keeps MVars from being completely subsumed by software transactional memory.

### Deadlocks

- thread A did `takeMVar a >> putMVar b`, thread B did `putMVar b >> takeMVar a`
  - Executes
    1. `takeMVar a` - `A`
    1. `putMVar b` - `B`
    1. `putMVar b` - `A` sleeps
    1. `takeMVar a` - `B` sleeps

Solution:
Always first `takeMVar`, then `putMVar`.

### Atomicity

If a thread does `takeMVar`, the `MVar` becomes empty, so no other thread may `takeMVar`.
Then, that thread does `putMVar`.
If the `MVar` modification was successful, the whole operation seems atomic to other threads.

From [Control.Concurrent.MVar](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Concurrent-MVar.html):

> In particular, the "bigger" functions in this module (swapMVar, withMVar, modifyMVar_ and modifyMVar) are simply the composition of a takeMVar followed by a putMVar with exception safety.
These have atomicity guarantees only if all other threads perform a takeMVar before a putMVar as well;
otherwise, they may block.




### Foreign calls

-- TODO

-}

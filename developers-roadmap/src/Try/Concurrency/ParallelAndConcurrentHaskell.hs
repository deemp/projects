{-# LANGUAGE ScopedTypeVariables #-}

module Try.Concurrency.ParallelAndConcurrentHaskell where

import Control.Concurrent ()
import Control.Exception (SomeException (SomeException), catch, mask, throw)
import qualified Data.Map as Map
import UnliftIO (MVar, atomically, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, newTMVarIO, putMVar, takeMVar, takeTMVar)

{-
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

### Exceptions

- Form a hierarchy
- Catching
  - Some handlers

      ```hs
      try :: Exception e => IO a -> IO (Either e a)
      handle :: Exception e => (e -> IO a) -> IO a -> IO a
      onException :: IO a -> IO b -> IO a
      ```
  - `onException` rethrows the exception. Example:

      ```hs
      bracket before after thing =
        mask $ \restore -> do
          a <- before
          r <- restore (thing a) `onException` after a
          _ <- after a
          return r
      ```
  - `catchJust`, `handleJust` select an exception by a predicate.

- Throwing
  - `throwIO :: Exception e => e -> IO a` guarantees the ordering of exceptions
  - `throw :: Exception e => e -> a` doesn't

- Processing actions with possible exceptions

    ```hs
    bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
    finally :: IO a -> IO b -> IO a
    ```

### Cancellation and Timeouts

#### Masking

```hs
mask :: ((IO a -> IO a) -> IO b) -> IO b
```

The mask operation defers the delivery of asynchronous exceptions for the duration of
its argument.

Here, asynchronous exceptions can only be delivered inside the argument of `restore` - inside `f a`.

-}

problem :: MVar a -> (a -> IO a) -> IO ()
problem m f = mask $ \restore -> do
  a <- takeMVar m
  r <- restore (f a) `catch` \(e :: SomeException) -> do putMVar m a; throw e
  putMVar m r

{-

mask is applied to a function, which takes as its argument a function restore. The
restore function can be used to restore the delivery of asynchronous exceptions to its
present state during execution of the argument to mask. If we imagine shading the entire
argument to mask except for the expression (f a), asynchronous exceptions cannot be
raised in the shaded portions.

#### Interruptibility

An `interruptible` operation may receive an asynchronous exception only if it actually blocks.
In the case of problem above, we know the MVar is definitely empty when we call putMVar,
so putMVar cannot block, which means that it is not interruptible.

#### Get current masking state

```hs
getMaskingState :: IO MaskingState

data MaskingState
  = Unmasked
  | MaskedInterruptible
  | MaskedUninterruptible
```

The getMaskingState function returns one of the following construc‐
tors:
- `Unmasked` - The current thread is not inside `mask` or `uninterruptibleMask`.
- `MaskedInterruptible` - The current thread is inside `mask`.
- `MaskedUninterruptible` - The current thread is inside `uninterruptibleMask`.

#### Multiple MVars

`modifyMVar` and company first `takeMVar`, then `putMVar`.
-}

modifyTwo :: MVar a -> MVar b -> (a -> b -> IO (a, b)) -> IO ()
modifyTwo ma mb f =
  modifyMVar_ mb $ \b ->
    modifyMVar ma $ \a -> f a b

{-
If this blocks in the inner modifyMVar and an exception is raised, then the outer modifyMVar_ will restore the contents of the MVar it took.

#### Bracket

```hs
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r
```

The IO actions passed in as before and after are performed inside mask. The bracket
function guarantees that if before returns, after will be executed in the future. It is
normal for before to contain a blocking operation; if an exception is raised while before
is blocked, then no harm is done. But before should perform only one blocking oper‐
ation. An exception raised by a second blocking operation would not result in after
being executed. If you need to perform two blocking operations, the right way is to nest
calls to bracket, as we did with modifyMVar.
Something else to watch out for here is using blocking operations in after. If you need
to do this, then be aware that your blocking operation is interruptible and might receive
an asynchronous exception.

### Timeouts

See
- [catchJust](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Exception-Base.html#v:catchJust)
- [timeout](https://hackage.haskell.org/package/base-4.18.0.0/docs/System-Timeout.html#v:timeout)
- [throwTo](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Concurrent.html#v:throwTo)
  - it's **always** interruptible

### Catching asynchronous exceptions

If you need to handle asynchronous exceptions, it’s usually important
for the exception handler to be inside a mask so that you don’t get interrupted by another
asynchronous exception before you’ve finished dealing with the first one. For that rea‐
son, catch or handle might be more appropriate, because you can take advantage of
the built-in mask.

Don't handle exceptions inside a handler.

### mask and forkIO

Use `async`

## Software Transactional memory

### MVar

There may be a situation when *have* to take 2 `MVar`s and need to order taking.

- Thread 1 takes the MVar for desktop a.
- Thread 2 takes the MVar for desktop b.
- Thread 1 tries to take the MVar for desktop b and blocks.
- Thread 2 tries to take the MVar for desktop a and blocks.

### STM

Can make transactions.

The current thread is blocked until one of the TVars that it is reading is written to,
at which point the thread is unblocked again and the transaction is rerun.

```hs
retry :: STM a
```

The meaning of `retry` is simply “abandon the current transaction and run it again.”

-}
p :: IO (Int, Int)
p = do
  ta <- newTMVarIO 2
  tb <- newTMVarIO 3
  atomically $ do
    a <- takeTMVar ta
    b <- takeTMVar tb
    return (a, b)

{-

This example is difficult to program with MVar because taking a single MVar is a side
effect that is visible to the rest of the program, and hence cannot be easily undone if the
other MVar is empty. One way to implement it is with a third MVar acting as a lock to
control access to the other two, but then of course all other clients have to be aware of
the locking protocol.

### Merging with STM

```hs
orElse :: STM a -> STM a -> STM a
```

Combine two blocking transactions such that one is performed but not both.

The operation orElse a b has the following behavior:
- First, a is executed. If a returns a result, then the orElse call returns it and ends.
- If a calls retry instead, a’s effects are discarded_ and b is executed instead.

### Async exceptions

Exceptions just discard transactions.

### Foreign calls

-- TODO

-}

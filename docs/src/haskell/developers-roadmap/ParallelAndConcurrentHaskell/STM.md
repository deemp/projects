```haskell
{-# LANGUAGE NumericUnderscores #-}

module Try.ParallelAndConcurrentHaskell.STM where

import UnliftIO
import UnliftIO.Concurrent
```

## Software Transactional memory

### MVar

There may be a situation when *have* to take 2 `MVar`s and need to order taking.

- Thread 1 takes the MVar for desktop a.
- Thread 2 takes the MVar for desktop b.
- Thread 1 tries to take the MVar for desktop b and blocks.
- Thread 2 tries to take the MVar for desktop a and blocks.

Deadlock

### STM

Can make transactions.

The current thread is blocked until one of the TVars that it is reading is written to,
at which point the thread is unblocked again and the transaction is rerun.

```hs
retry :: STM a
```

The meaning of `retry` is simply “abandon the current transaction and run it again.”

```haskell
p :: IO (Int, Int)
p = do
  ta <- newTMVarIO 2
  tb <- newTMVarIO 3
  atomically $ do
    a <- takeTMVar ta
    b <- takeTMVar tb
    return (a, b)
```

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

## Async

- With `async`, the calling thread isn't blocked when running an **async** action.
- We can check `Async a` for a result or block on it
- `withAsync :: IO a -> (a -> IO b) -> IO b` - when the function `a -> IO b` returns, `IO a` is killed.
  - There's no contradiction. We can't use the value stored in `a` without calling `wait a`. But this will make the computation `IO b` to suspend until `IO a` finishes or throws an exception.

      <!-- i 6 -->

      ```haskell
      exAsync = withAsync (threadDelay 3_000_000 >> print "ping") (\a -> wait a >> print "pong")
      ```

- `retry` restarts a transaction and blocks the thread until one of the variables that were read changes its value

- `Broadcasting` channel (e.g., `TMChan`)

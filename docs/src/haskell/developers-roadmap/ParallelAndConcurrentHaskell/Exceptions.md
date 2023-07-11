```haskell
module Try.ParallelAndConcurrentHaskell.Exceptions where

import UnliftIO
```

## Exceptions

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

```haskell
problem :: MVar a -> (a -> IO a) -> IO ()
problem m f = mask $ \restore -> do
  a <- takeMVar m
  r <- restore (f a) `catchAny` \e -> do putMVar m a; throwIO e
  putMVar m r
```

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

```haskell
modifyTwo :: MVar a -> MVar b -> (a -> b -> IO (a, b)) -> IO ()
modifyTwo ma mb f =
  modifyMVar_ mb $ \b ->
    modifyMVar ma $ \a -> f a b
```

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

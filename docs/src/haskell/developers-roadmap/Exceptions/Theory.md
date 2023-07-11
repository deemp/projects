```haskell
module Try.Exceptions.Theory where
```

### Exceptions

- [Safe exception handling](https://www.fpcomplete.com/haskell/tutorial/exceptions/)
  - Types of exceptions:
    - `synchronous` - generated in `IO`, thrown inside a single thread. Allow `recovery` and `cleanup`
    - `asynchronous` - generated from outside a thread. Allow `cleanup`, but `no recovery`
    - `impure` - generated in a pure code, thrown when a thunk gets evaluated.
      - Example: `error`
- [Exceptions and concurrency] - [YT](https://www.youtube.com/watch?v=UKAGN8v2t2k)
  - use [safe-exceptions]

From Haskell in Depth (Chapter 7)

- Avoid using exceptions when possible
- Due to laziness, an exception in an expression doesn't happen until that expression gets evaluated.
- A thread can be killed by other threads or the runtime system (due to memory exhaustion, etc.)
- Types of exceptions:
  - `programmable`
    - used in pure monad stacks (e.g., `ExceptT`)
  - `extensible`
    - Extensions thrown anywhere, caught only in `IO`
    - Supported by the GHC runtime system
    - `imprecise` exceptions - thrown in pure code, order unspecified by the runtime system
    - All exceptions have an instance of `Exception` and are values of the `SomeException` type
- packages - `exceptions`, `safe-exceptions`

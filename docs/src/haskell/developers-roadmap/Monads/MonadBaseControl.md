```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use gets" #-}
{-# HLINT ignore "Use asks" #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Try.Monads.MonadBaseControl where

import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get, put), MonadTrans (lift), StateT (runStateT))
import Control.Monad.Trans.Writer
import Data.Functor ((<&>))
```

https://lexi-lambda.github.io/blog/2019/09/07/demystifying-monadbasecontrol/

## The essence of MonadBaseControl

- Capture the action’s input state and close over it.
- Package up the action’s output state with its result and run it.
- Restore the action’s output state into the enclosing transformer.
- Return the action’s result.

```haskell
class MonadBase b m => MonadBaseControl b m | m -> b where
  type InputState m
  type OutputState m
  captureInputState :: m (InputState m)

  -- run monad with an input state and return a result and the output state in another monad
  -- we have access to the result of the first monad
  closeOverInputState :: m a -> InputState m -> b (a, OutputState m)
  restoreOutputState :: OutputState m -> m ()

instance MonadBaseControl IO IO where
  type InputState IO = ()
  type OutputState IO = ()
  captureInputState = pure ()
  closeOverInputState m () = m <&> (,())
  restoreOutputState () = pure ()

instance MonadBaseControl b m => MonadBaseControl b (StateT s m) where
  type InputState (StateT s m) = (s, InputState m)
  type OutputState (StateT s m) = (s, OutputState m)
  captureInputState = (,) <$> get <*> lift captureInputState
  closeOverInputState m (s, ss) = do
    ((v, s'), ss') <- closeOverInputState (runStateT m s) ss
    pure (v, (s', ss'))
  restoreOutputState (s, ss) = lift (restoreOutputState ss) *> put s

instance MonadBaseControl b m => MonadBaseControl b (ReaderT r m) where
  type InputState (ReaderT r m) = (r, InputState m)
  type OutputState (ReaderT r m) = OutputState m
  captureInputState = (,) <$> ask <*> lift captureInputState
  closeOverInputState m (r, s) = closeOverInputState (runReaderT m r) s
  restoreOutputState s = lift (restoreOutputState s)

instance (MonadBaseControl b m, Monoid w) => MonadBaseControl b (WriterT w m) where
  type InputState (WriterT w m) = InputState m
  type OutputState (WriterT w m) = (w, OutputState m)
  captureInputState = lift captureInputState
  closeOverInputState m ss = do
    ((v, s'), ss') <- closeOverInputState (runWriterT m) ss
    pure (v, (s', ss'))
  restoreOutputState (s, ss) = lift (restoreOutputState ss) *> tell s
```

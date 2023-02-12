{- FOURMOLU_DISABLE -}
{- LIMA_DISABLE -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}
{- FOURMOLU_ENABLE -}

module Main (main) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Monad.Fix (fix)
import Control.Monad.Identity (Identity)
import Data.Foldable (fold, Foldable (foldl'))
import Data.Kind (Type)
import Language.Haskell.TH.Syntax (Dec, Quasi, runQ)

main = undefined

{- LIMA_ENABLE -}

{-
# Developers roadmap

for backend - [src](https://github.com/fullstack-development/developers-roadmap)

## Prerequisites

- See `VSCodium` for `Haskell` [template](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme).
It explains what's available in this project.
- Next, see Haskell [Prerequisites](https://github.com/br4ch1st0chr0n3/flakes/blob/main/README/Haskell.md).
- Following that, see [Troubleshooting](https://github.com/br4ch1st0chr0n3/flakes/blob/main/README/Troubleshooting.md).
- Recurse into `Prerequisites` on the linked pages to get even more info.

## Quick start

1. Start a devshell

  ```terminal
  nix develop
  ```

1. (Optionally) Start `VSCodium`:

  ```terminal
  nix run .#writeSettings
  nix run .#codium .
  ```

1. Open a `README.hs` and hover over a function. `Haskell Language Server` should start giving you hints.

## Junior 1

[link](https://github.com/fullstack-development/developers-roadmap/blob/master/backend/junior-1#readme)

### Haskell

- [code](./src/Junior1.hs)

## Junior 2

### Type classes

#### Foldable

```hs
class Foldable t where
```

- When using folds, can force the evaluation of an accumulator
  - `deepseq`
  - [BangPatterns](http://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/bang-patterns.html) with pattern matching on the element of an accumulator to force.
-}

{- LIMA_INDENT 4 -}

-- >>> foldl (\(!a1, !a2) x -> (a1 + x, a2 + x)) (0, 0) [1..9]
-- (45,45)

{- LIMA_DEDENT -}

{-
- `foldl'` - fold a list from the left: `f (f (f x a1) a2) ...` and have accumulator in WHNF.
- `foldr` - calculate the full list and fold it from the right: `f (f (f x a5) a4) ...`.
  - Can terminate early if an operation is strict in the left argument (like `&&`) - [SO](https://stackoverflow.com/a/27682341)
-}

{- LIMA_INDENT 4 -}

-- >>> foldr (&&) False (repeat False)
-- False

{- LIMA_DEDENT -}

{-
- `fold :: (Foldable t, Monoid m) => t m -> m`
  - folds a container with elements that have a `Monoid` instance
-}

{- LIMA_INDENT 4 -}
-- >>> fold [Just "a", Nothing, Just "c"]
-- Just "ac"
{- LIMA_DEDENT -}

{-
- `foldMap` - maps each element to a `Monoid` and `fold`s the container
-}

{- LIMA_INDENT 4 -}
-- >>> foldMap Just ["a", "b", "c"]
-- Just "abc"
{- LIMA_DEDENT -}

{-
#### Alternative and MonadPlus

- [Haskell wikibooks](https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus):
-}

{-
  - `Alternative`
    - Definition

      ```hs
      class Applicative f => Alternative f where
        empty :: f a
        (<|>) :: f a -> f a -> f a
      ```

    - There's no instance for `Either a`
    - As it's an associative operation, it produces the same result for either fold
-}

{- LIMA_INDENT 6 -}

-- >>> foldr (<|>) empty [Just "a", Nothing, Just "c", Nothing, Just "e"]
-- Just "a"

-- >>> foldl' (<|>) empty [Just "a", Nothing, Just "c", Nothing, Just "e"]
-- Just "a"

{- LIMA_DEDENT -}

{-
#### Traversable

- [Haskell wikibooks](https://en.wikibooks.org/wiki/Haskell/Traversable):
-}

{- LIMA_INDENT 4 -}

class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)

  -- These methods have default definitions.
  -- They are merely specialised versions of the other two.
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)

{- LIMA_DEDENT -}

{-
#### Handle pattern

- [src](https://www.metalamp.ru/articles/service-handle-pattern)

- Take functions from a **given** environment, e.g. from `ReaderT`

#### Exceptions

- [Safe exception handling](https://www.fpcomplete.com/haskell/tutorial/exceptions/)
  - Types of exceptions:
    - `synchronous` - generated in `IO`, thrown inside a single thread. Allow `recovery` and `cleanup`
    - `asynchronous` - generated from outside a thread. Allow `cleanup`, but `no recovery`
    - `impure` - generated in a pure code, thrown when a thunk gets evaluated.
      - Example: `error`

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

#### Concurrency

##### STM

- With `async`, the calling thread isn't blocked when running an **async** action.
- We can check `Async a` for a result or block on it - see [TryAsync](./app/TryAsync.hs)
- `withAsync :: IO a -> (a -> IO b) -> IO b` - when the function `a -> IO b` returns, `IO a` is killed.
  - There's no contradiction. We can't use the value stored in `a` without calling `wait a`. But this will make the computation `IO b` to suspend until `IO a` finishes or throws an exception.
-}
{- LIMA_INDENT 6 -}

exAsync = withAsync (threadDelay 3_000_000 >> print "ping") (\a -> wait a >> print "pong")

{- LIMA_DEDENT -}

{-
- `retry` restarts a transaction and blocks the thread until one of the variables that were read changes its value

- `Broadcasting` channel (e.g., `TMChan`)

##### Non-STM

- [Pessimistic and optimistic locking](https://stackoverflow.com/questions/129329/optimistic-vs-pessimistic-locking)

#### Testing

-*HiD, Chapter  8**
Test types:

- `unit` tests -
- `property` tests -
- `golden` tests - check against a reference file contents (`tasty-golden`)

## Junior 3

[link](https://github.com/fullstack-development/developers-roadmap/tree/master/backend/junior-3#readme)

### Haskell

[code](./src/Junior3.hs)

#### Type classes

- The part before the `=>` is the context, while the part after the `=>` is the head of the instance declaration. - [src](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=overlapping%20instances#instance-declarations-and-resolution)

    ```hs
    instance (assertion1, ..., assertionn) => class type1 ... typem where ...
    ```

- How are type classes implemented in Haskell?
  - What is a dictionary?
    - A data type with class functions as fields
  - How is it defined and passed into functions? - [src](https://arxiv.org/pdf/1907.00844.pdf#subsection.2.1)
    - embed `Superclass` dictionary into `Subclass` dictionary

-}

{- LIMA_INDENT 6 -}

newtype BaseD a = BaseD {base :: a -> Bool}
data Sub1D a = Sub1D
  { super1 :: BaseD a
  , sub1 :: a -> Bool
  }

{- LIMA_DEDENT -}

{-
    - Passed automatically by the compiler
- Why using constraints on a type variable within a data declaration isn't a good idea?
  - They make code less flexible and disallow some instances - [SO](https://stackoverflow.com/a/40825913)
  - Can be achieved by using `GADTs`
- What is coherence and why is it important to maintain it? What are the possible cases of coherence violation?
  - > A program is coherent if it has exactly one meaning — i.e., its semantics is unambiguously determined.
  - `Coherence` is when multiple `type derivations` are possible - [SO](https://stackoverflow.com/a/68008592)
  - For each different derivation a different class instance can be used. This may lead to different behaviors
  - `FlexibleInstances` and `MultiParamTypeClasses` introduce incoherence
  - Need to maintain coherence to write a program whose type checking (`static`) doesn't change its runtime (`dynamic`) properties
- Overlapping
  - How does the instance selection process happen?
    - Find an instance with satisfying `B` of (`instance A => C B where`)
    - Find instance for `A`
  - Is it possible to have overlapping instances?
    - `instance C a` and `instance C Bool`
  - Does having overlapping instances violate coherence?
    - No
  - Basics of Haskell instance selection - [src](https://www.youtube.com/watch?v=XfIlhJFmw3c)
  - Is it possible to have a compiled and working program with coherence violations?
    - Yes - [src](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=overlapping%20instances#instance-signatures-type-signatures-in-instance-declarations) (see example above)
  - How would you solve a problem of overlapping instances in various situations?
    - Make the most specific instance discoverable using the fine-grained per-instance pragmas
    - Rewrite
      - `instance {-# OVERLAPPABLE #-} C a` and `instance C Bool`
      - `instance C a` and `instance {-# OVERLAPPING #-} C Bool`
      - `OVERLAPS` = both
- Orphans
  - What are orphan instances? Why are they undesirable?
    - An orphan instance is a type class instance for class C and type T which is neither defined in the module where C is defined nor in the module where T is defined. - [src](https://wiki.haskell.org/Orphan_instance)
    - Type class instances are special in that they don't have a name and cannot be `imported` explicitly. This also means that they cannot be `excluded` explicitly. All instances defined in a module `A` are imported automatically when importing `A`, or importing any module that imports `A`, directly or indirectly.
    - Orphans may break the functionality planned by the library author
    - Orphans invalidate file fingerprints (hash of a file made by GHC to check later if a file has changed) and transitively - in modules that import them - [src](https://tech.freckle.com/2018/12/12/a-home-for-orphan-instances/#fewer-dirty-fingerprints)
  - How to deliver orphans?
    - Expose type and instance only together by putting orphans into modules and re-exporting them - [src](https://www.michaelpj.com/blog/2020/10/29/your-orphans-are-fine.html#private-modules)
      - Cons:
        - a user has to use your instances
        - your lib uses more dependencies
    - Define instances in a separate package - [src](https://www.michaelpj.com/blog/2020/10/29/your-orphans-are-fine.html#private-packages)
      - cons: need to track these packages
  - Does having orphan instances violate coherence?
    - When orphans violate coherence:
      - If you actually import both instances, your program will fail to compile.
      - If you do not directly import both, but rather use two modules which independently use the differing instances, you can end up with incoherent behaviour.
  - What are the pros and cons of isolating orphans in special modules?
    - Pros: less often fingerprints invalidation
    - Cons: need to recompile the whole project on changes in that module - [src](https://tech.freckle.com/2018/12/12/a-home-for-orphan-instances/#decrease-the-surplus-compilation)
- How the problem of orphans and overlapping is solved in other languages or by different overloading implementation techniques?
  - Scala
    - An orphan instance in Scala means an instance that exists neither in the type's companion object nor the type class' companion object - [src](https://pjrt.medium.com/orphan-instances-in-scala-322caa78e382)
    - Import packages with type and instance declaration separately
- What are the problems of current typeclasses implementation?
  - There's no formal proof that instance resolution is coherent
- Is there a problem of structuring the hierarchy of standard typeclasses?
  <!-- TODO -->
- What is Final Tagless (FT) style? - [src](https://serokell.io/blog/introduction-tagless-final)
  - Example:
    - `wimble :: (MonadReader Env m, MonadState State m) => m ()`
  - Can extend in two dimensions
      1. a new interpreter (change implementation of `MonadReader`)
      2. a new set of operations (add a constraint like `MonadWriter`)
  - `Application monad` (`AM`) - a monad for organizing effectful application code
    - `FT` can define `AM`
  - `Tagged Initial` - sum types are represented as `(tag, payload)`. `tag` - for pattern-matching
  - `Tagless Initial` - use `GADTs` to ban nonsense expressions, no tags
  - `Final Tagless` - use overloaded functions

- `Functor` laws:

  ```hs
  fmap id = id
  ```

  ```hs
  fmap (f . g)  ==  fmap f . fmap g
  ```

### Type and Data Families

Haskell wiki ([src](https://wiki.haskell.org/GHC/Type_families#What_are_type_families.3F)):

> The concept of a type family comes from type theory. An indexed type family in type theory is a partial function at the type level.
Applying the function to parameters (called type indices) yields a type.
Type families permit a program to compute what data constructors it will operate on,
rather than having them fixed statically (as with simple type systems) or treated as opaque unknowns
(as with parametrically polymorphic types).

> Type families are to vanilla data types what type class methods are to regular functions.
Vanilla polymorphic data types and functions have a single definition, which is used at all type instances.
Classes and type families, on the other hand, have an interface definition and any number of instance definitions.
A type family's interface definition declares its kind and its arity, or the number of type indices it takes.
Instance definitions define the type family over some part of the domain.

- **Type Families: The Definitive Guide** - [src](https://serokell.io/blog/type-families-haskell)
  - `Non-generative` type can be reduced to other types:
    - `Pair a` -> `(a, a)`
    - Non-generative type constructors have arities assigned to them and must be used saturated.

  - `Generative` type constructor - can't be reduced to another type
    - `Maybe Bool ~ Maybe Bool` and nothing else
    - We set the `kind` via a standalone `type ...`. Here, `MaybeIf` requires something of kind `Bool` for construction. Therefore, we supply a promoted `True` to it.
-}

{- LIMA_INDENT 6 -}

type MaybeIf :: Bool -> Type -> Type
type family MaybeIf b t where
  MaybeIf True t = Maybe t
  MaybeIf False t = Identity t

{-
  - Use to implement operations on `GADTs` (e.g., concatenate lists)
-}

type HList :: [Type] -> Type
data HList xs where
  HNil :: HList '[]
  (:&) :: x -> HList xs -> HList (x : xs)
infixr 5 :&

type Append :: [a] -> [a] -> [a]
type family Append xs ys where -- header
  Append '[] ys = ys -- clause 1
  Append (x : xs) ys = x : Append xs ys -- clause 2
happend :: HList xs -> HList ys -> HList (Append xs ys)
happend = undefined

{- LIMA_DEDENT -}

{-
  - **Closed type families**
    - The clauses of a closed type family are ordered and matched **from top to bottom**
    - Overlapping equations
-}

{- LIMA_INDENT 8 -}
type And :: Bool -> Bool -> Bool
type family And a b where
  And True True = True
  And _ _ = False

{- LIMA_DEDENT -}

{-
  - **Open type families**
    - Such families can be extended anywhere
    - The equations of an open type family are either:
      - Not overlapping, so get a combinatorial explosion in patterns:
-}

{- LIMA_INDENT 8 -}

type And' :: Bool -> Bool -> Bool
type family And' a b

type instance And' True True = True
type instance And' True False = False
type instance And' False True = False
type instance And' False False = False

{- LIMA_DEDENT -}

{-
      - Compatible:
        - Can make right sides equal and unify left sides via rewriting
-}

{- LIMA_INDENT 10 -}
type family G a b

type instance G a Bool = a -> Bool
type instance G Char b = Char -> b

-- ==>

type instance G Char Bool = Char -> Bool

{- LIMA_DEDENT -}

{-
  - **Associated types**
    - Almost the same as open type families
    - Can set default values
-}

{- LIMA_INDENT 6 -}

type family Unwrap x where
  Unwrap (f a) = a

class Container2 a where
  type Elem2 a

  -- default
  type Elem2 x = Int
  elements' :: a -> [Elem2 a]

{- LIMA_DEDENT -}

{-
    - Example from [string-interpolate](https://williamyaoh.com/posts/2019-05-27-string-interpolation-and-overlapping-instances.html#cb12)

  - **Injectivity** - get input types from output types
    - Use `TypeFamilyDependencies`
-}

{- LIMA_INDENT 8 -}
type family Not x = r | r -> x where

-- >>> s @True

{- LIMA_DEDENT -}

{-
  - **Data families**
    - [HaskellWiki](https://wiki.haskell.org/GHC/Type_families#Detailed_definition_of_data_families)
    - Compute **new** data types (type families compute the existing data types)
-}

{- LIMA_INDENT 6 -}
data family Vector a
newtype instance Vector () = VUnit Int
newtype instance Vector Int = VInts [Int]

{- LIMA_DEDENT -}

{-
    - Can associate with a class
-}
{- LIMA_INDENT 8 -}
class Vectorizable a where
  data Vector_ a
  vlength :: Vector_ a -> Int

newtype S = S {unS :: [Int]}
instance Vectorizable S where
  data Vector_ S = Vector_ {unVector_ :: S}
  vlength :: Vector_ S -> Int
  vlength = length . unS . unVector_

{- LIMA_DEDENT -}

{-
### GADTs

- Wikibooks ([src](https://en.wikibooks.org/wiki/Haskell/GADT)):
  > With GADTs, a constructor for `Foo` a is not obliged to return `Foo a`; it can return any `Foo blah` that you can think of:
-}

{- LIMA_INDENT 4 -}

data TrueGadtFoo a where
  MkTrueGadtFoo :: a -> TrueGadtFoo Int

{- LIMA_DEDENT -}

{-
  - Still, need to use a relevant data constructor

    ```hs
    data Foo where
      MkFoo :: Bar Int-- This will not typecheck
    ```

- Is it considered a good practice to put constraints in consructors inside GADT declaration?
  - No. Use a compiler to derive instances like `Functor`, put constraints in functions

- How can heterogenous lists be implemented with DataKinds and GADTs? - [src](https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html#heterogeneous-lists)
-}

{- LIMA_INDENT 4 -}

data HList_ xs where
  HNil_ :: HList_ '[]
  (:::) :: a -> HList_ as -> HList_ (a ': as)

infixr 6 :::

hex :: HList_ '[Char, Integer, String]
hex = 'a' ::: 1 ::: "hello" ::: HNil_

{- LIMA_DEDENT -}

{-
### Kinds

- `DataKinds` - [src](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_data.html#type-level-data-declarations)
  - What is the data type promotion?
    - promote terms to type level like `'[1,2,3]` - [src]
- Are types with promoted kinds inhabited?
  - **inhabited types** (types that have at least 1 value) are of kind **Type**
- `ConstraintKinds` - `Constraint`s as first-class citizens

  ```hs
  type Stringy a = (Read a, Show a)
  ```

### Functional dependencies

- Set a relation between types. Make one type correspond to another type

-}

{- LIMA_INDENT 4 -}

class Monad m => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

{- LIMA_DEDENT -}

{-
- Problem ([src](https://www.fpcomplete.com/haskell/tutorial/fundeps/#exercises)):
  > we want a MonadReader typeclass where there is only a single instance per m, and we know the env parameter that will be available from each m.
  - Approach 1:
    - `MultiParamTypeClasses`) let us specify explicitly what the `env` is
    - `FunctionalDependencies` allow us to constrain ourselves to a single instance.

      ```hs
      newtype PersonReader a = PersonReader { runPersonReader :: Person -> a } deriving Functor

      class MonadReader env m | m -> env where
        ask :: m env

      instance MonadReader Person PersonReader where
        ask = PersonReader $ \env -> env

      instance MonadReader env (Reader env) where
        ask = Reader $ \env -> env

      greeting :: PersonReader String
      greeting = do
        person <- ask
        -- Here, derives that `person :: Person`
        -- from `instance MonadReader Person PersonReader`
        -- via fundep `m -> env` and `ask :: m env`
        pure $ show person
      ```

    - Approach 2:
      - `TypeFamilies`
-}

{- LIMA_INDENT 10 -}
class MonadReader m where
  -- use an associated type
  type Env m
  ask :: m (Env m)

data Person
newtype PersonReader a = PersonReader (a -> a)

-- `m (Env m)` calculates to `PersonReader Person`
instance MonadReader PersonReader where
  type Env PersonReader = Person
  ask :: PersonReader (Env PersonReader)
  ask = PersonReader id

{- LIMA_DEDENT -}

{-
### Laziness

- `Bang patterns`

  ```hs
  {-# LANGUAGE BangPatterns #-}

  add :: Int -> Int -> Int
  add !x !y = x + y

  -- equivalent to
  add x y = x `seq` y `seq` x + y
  ```

- `$!` - strict application
- `Thunk` is an unevaluated expression - [src](https://stackoverflow.com/a/13984345)
  - `free variables` in an unevaluated expr
  - when evaluated, the pointers to it will point to the result
  - a `dead thunk` is `garbage collected`

- Expression forms - [src](https://stackoverflow.com/a/6889335)
  - Normal form
    > An expression in normal form is fully evaluated, and no sub-expression could be evaluated any further (i.e. it contains no un-evaluated thunks).
  - Weak head normal form
    > An expression in weak head normal form has been evaluated to the outermost data constructor or lambda abstraction (the head).

    ```hs
    (1 + 1, 2 + 2)       -- the outermost part is the data constructor (,)
    \x -> 2 + 2
    ```

- `` a `seq` b `` - eval `a` to `WHNF`, return `b`
- `` a `deepseq` b `` - eval `a` to `NF`, return `b`
- `` force b = b `deepseq` b `` - eval `b` to `NF` and return `b`
  - If we have `let a = force b`, `a` is not in `NF`
  - To get `a` in `NF`, we need to `!a`
- [Thunks, Sharing, Laziness](https://youtu.be/I4lnCG18TaY) via `ghc-viz` (available in `nixpkgs`)

### Fix combinator
-}

ex13 :: [Int] -> Int
ex13 = fix (\t c -> \case (a0 : a1 : as) -> t (c + fromEnum (signum a0 /= signum a1)) (a1 : as); _ -> c) 0

-- >>>ex13 [-3,0,2,0,5]
-- 4

{-
### File IO

- There are several representations of text in `Haskell` - `ByteString`, `Text`, `String`
- `ByteString` can contain both `human-readable` or `binary` data that mustn't be mixed
- Also, there are many `file encodings`. Use `UTF-8` to be safe
- One can encode standard data types into a `ByteString` using [Data.ByteString.Builder](https://hackage.haskell.org/package/bytestring-0.11.4.0/docs/Data-ByteString-Builder.html)
- `LBS` reads files in chunks. Can be used for streaming
- `hGet` reads a given number of bytes from a handle
- `stdout` and `stdin` are files
- Can set buffering mode on a handle: `hSetBuffering stdout NoBuffering`

### Debugging

- `Debug.Trace`
- `breakpoint` - [src](https://github.com/aaronallen8455/breakpoint)
  - put breakpoints into an app (see [TryBreakpoint](./app/TryBreakpoint.hs))
  - inspect variables visible at a breakpoint
  - freeze other threads (`GHC 9.2.x+`)

### Monoid

- [List comprehension](https://wiki.haskell.org/List_comprehension)
  - Skip elements
    - On a flag

        ```hs
        ["--deepClone" | _deepClone]
        ```

    - On a pattern fail

        ```hs
        catMaybes :: [Maybe a] -> [a]
        catMaybes ls = [x | Just x <- ls]
        ```

### Template Haskell
-}

ex2 :: (Quasi a) => a [Dec]
ex2 = runQ [d|decl :: Int; decl = 1 + 2|]

-- >>>ex2
-- [SigD decl_0 (ConT GHC.Types.Int),ValD (VarP decl_0) (NormalB (InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2))))) []]

{-
## Misc

- Parsing with Haskell
  - [Part 1](https://serokell.io/blog/lexing-with-alex)
- Haskell CI with caching - [src](https://github.com/aaronallen8455/breakpoint/blob/main/.github/workflows/ci.yml)
- `string-interpolate` - [src](https://hackage.haskell.org/package/string-interpolate)
  - UTF-8 string interpolation
- [View Patterns](https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns)
-}

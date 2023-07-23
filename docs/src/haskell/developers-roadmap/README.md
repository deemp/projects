# Developers roadmap

Inspired by [developers-roadmap](https://github.com/fullstack-development/developers-roadmap).

Extensions:

<!-- FOURMOLU_DISABLE -->

```haskell
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
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
```

<!-- FOURMOLU_ENABLE -->

Imports

```haskell
import Control.Monad.Fix (fix)
import Language.Haskell.TH.Syntax (Dec, Quasi, runQ)
```

<!-- d -->

```haskell
main = undefined
```

<!-- e -->

## Kinds

- `DataKinds` - [src](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_data.html#type-level-data-declarations)
  - What is the data type promotion?
    - promote terms to type level like `'[1,2,3]`
- Are types with promoted kinds inhabited?
  - **inhabited types** (types that have at least 1 value) are of kind **Type**
- `ConstraintKinds` - `Constraint`s as first-class citizens

  ```hs
  type Stringy a = (Read a, Show a)
  ```
- `Symbol` - a compile-time string
  - [UnconsSymbol](https://hackage.haskell.org/package/base-4.18.0.0/docs/GHC-TypeLits.html#t:UnconsSymbol)
  - [typed-interpolation](https://github.com/dmjio/typed-interpolation) - a good parsing example.

## Functional dependencies

- Set a relation between types. Make one type correspond to another type

    <!-- i 4 -->

    ```haskell
    class (Monad m) => MonadError e m | m -> e where
      throwError :: e -> m a
      catchError :: m a -> (e -> m a) -> m a
    ```

- Problem ([src](https://www.fpcomplete.com/haskell/tutorial/fundeps/#exercises)):
  > we want a MonadReader typeclass where there is only a single instance per m, and we know the env parameter that will be available from each m.
  - Approach 1:
    - `MultiParamTypeClasses` let us specify explicitly what the `env` is
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

          <!-- i 10 -->

          ```haskell
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
          ```

## Laziness

- `Bang patterns`

  <!-- i 2 -->

<!-- FOURMOLU_DISABLE -->

  ```haskell
  {-# LANGUAGE BangPatterns #-}
  ```

<!-- FOURMOLU_ENABLE -->

  ```haskell
  addBang :: Int -> Int -> Int
  addBang !x !y = x + y
  
  -- equivalent to
  addSeq :: Int -> Int -> Int
  addSeq x y = x `seq` y `seq` x + y
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

- [safe-exceptions](https://www.fpcomplete.com/haskell/tutorial/exceptions/)
  - force impure exceptions using `tryAnyDeep` and `NFData`.

## Fix combinator

  ```haskell
  ex13 :: [Int] -> Int
  ex13 =
    fix
      ( \t c ->
          \case
            (a0 : a1 : as) -> t (c + fromEnum (signum a0 /= signum a1)) (a1 : as)
            _ -> c
      )
      0
  
  -- >>>ex13 [-3,0,2,0,5]
  -- 4
  ```

## File IO

- There are several representations of text in `Haskell` - `ByteString`, `Text`, `String`
- `ByteString` can contain both `human-readable` or `binary` data that mustn't be mixed
- Also, there are many `file encodings`. Use `UTF-8` to be safe
- One can encode standard data types into a `ByteString` using [Data.ByteString.Builder](https://hackage.haskell.org/package/bytestring-0.11.4.0/docs/Data-ByteString-Builder.html)
- `LBS` reads files in chunks. Can be used for streaming
- `hGet` reads a given number of bytes from a handle
- `stdout` and `stdin` are files
- Can set buffering mode on a handle: `hSetBuffering stdout NoBuffering`

## Debugging

- `Debug.Trace`
- `breakpoint` - [src](https://github.com/aaronallen8455/breakpoint)
  - put breakpoints into an app (see [TryBreakpoint](./app/TryBreakpoint.hs))
  - inspect variables visible at a breakpoint
  - freeze other threads (`GHC 9.2.x+`)

## Monoid

- [List comprehension](https://wiki.haskell.org/List_comprehension)
  - Skip elements
    - On a flag

        <!-- i 8 -->

        ```haskell
        _deepClone :: Bool
        _deepClone = True
        
        s1 :: [String]
        s1 = ["--deepClone" | _deepClone]
        ```

    - On a pattern fail

        <!-- i 8 -->

        ```haskell
        catMaybes :: [Maybe a] -> [a]
        catMaybes ls = [x | Just x <- ls]
        ```

## Template Haskell

- [capture haddocks](https://github.com/codedownio/aeson-typescript/blob/671347e3739b63bf04d5412330dc9a4748c7832e/src/Data/Aeson/TypeScript/Util.hs#L224)
  - [getDoc](https://hackage.haskell.org/package/template-haskell-2.19.0.0/docs/Language-Haskell-TH-Syntax.html#v:getDoc)

- print the structure of an expression

  <!-- i 2 -->

  ```haskell
  ex2 :: (Quasi a) => a [Dec]
  ex2 = runQ [d|decl :: Int; decl = 1 + 2|]
  
  -- >>>ex2
  -- [SigD decl_0 (ConT GHC.Types.Int),ValD (VarP decl_0) (NormalB (InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2))))) []]
  ```

## Higher-Kinded Data

- Defaulting fields in a record (via HKD) - [GH](https://gist.github.com/chrisdone/7dddadd089e6a5d2e3e9445c4692d2c2)
- [Higher-Kinded Data](https://reasonablypolymorphic.com/blog/higher-kinded-data/)
  - [Free Lenses for Higher-Kinded Data](https://reasonablypolymorphic.com/blog/free-lenses/index.html)
  - [HKD: Less Terrible than You Might Expect](https://reasonablypolymorphic.com/blog/hkd-not-terrible/index.html)
  - and [others](https://reasonablypolymorphic.com/)

## Generics

- [Higher-Kinded Data](https://reasonablypolymorphic.com/blog/higher-kinded-data/)
- `aeson` converts data to generic representation.
  - Its functions for parsing use selector names, modify them via options, then convert to or parse JSON.

## QualifiedDo

- [Qualified do: rebind your do-notation the right way](https://www.tweag.io/blog/2020-07-13-qualified-do-announcement/)
  - example

    ```hs
    {-# LANGUAGE QualifiedDo #-}

    module Main where

    import Data.Function((&))

    (>>=) = (&)

    foo :: Int
    foo = Main.do
      z <- (3, 4)
      (x, s) <- z
      x

    main = print foo
    ```

## Effects

### Effectful

- [effectful](https://hackage.haskell.org/package/effectful)
  - [Talk](https://www.youtube.com/watch?v=BUoYKBLOOrE) at Lambda
  - Сервер с servant, esqueleto, effectful - [YT](https://youtube.com/playlist?list=PLDtVwbUDS3Wky1MaMPqFp0eaOGd_gzvZo)
  - News site back-end - [GH](https://github.com/breaking-news-org/back-end)
  - Effects may be pure - `runPureEff`

## String interpolation
- [string-interpolate](https://hackage.haskell.org/package/string-interpolate)
- [nyan-interpolation](https://hackage.haskell.org/package/nyan-interpolation)
- [PyF](https://hackage.haskell.org/package/PyF)
- [typed-interpolation](https://github.com/dmjio/typed-interpolation)

## Optics
- [Optics are monoids](https://www.haskellforall.com/2021/09/optics-are-monoids.html)
- [Optics by example](../optics-by-example)

## Monad transformer stack

- Determine the type - [SO](https://stackoverflow.com/a/13724465)

## UnliftIO

- [Demystifying MonadBaseControl](https://lexi-lambda.github.io/blog/2019/09/07/demystifying-monadbasecontrol/)
  - Capture the action’s input state and close over it.
  - Package up the action’s output state with its result and run it.
  - Restore the action’s output state into the enclosing transformer.
  - Return the action’s result.

## Handle pattern

- [src](https://www.metalamp.ru/articles/service-handle-pattern)

- Take functions from a **given** environment, e.g. from `ReaderT`

## Data

- [large-records](https://github.com/well-typed/large-records)
- Avoid quadratic Core size - [advice](https://well-typed.com/blog/2021/10/large-records-part-2/#tldr-advice)

## GHCJS

- [rzk-lang/rzk](https://github.com/rzk-lang/rzk) - see `flake.nix`

## Nix

- To keep completions in [share](https://github.com/NixOS/cabal2nix/issues/433#issuecomment-862557347), need to modify [justStaticExecutables](https://github.com/NixOS/nixpkgs/blob/c032f4a16c1d09533c8af71002d5e7ad2d85af60/pkgs/development/haskell-modules/lib/compose.nix#L288C8-L288C8) 
so that it doesn't remove `share`.

## Misc

- [Радости и горести побед над C: делаем конфетку из прототипа wc на хаскеле](https://habr.com/ru/articles/496370/)
  - [hwc](https://github.com/0xd34df00d/hwc/tree/master)
- Parsing with Haskell
  - [Part 1](https://serokell.io/blog/lexing-with-alex)
- Haskell CI with caching - [src](https://github.com/aaronallen8455/breakpoint/blob/main/.github/workflows/ci.yml)
- `string-interpolate` - [src](https://hackage.haskell.org/package/string-interpolate)
  - UTF-8 string interpolation
- [ViewPatterns](https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns)

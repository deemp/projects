# Developers roadmap

for backend - [src](https://github.com/fullstack-development/developers-roadmap)

## Prerequisites

* See `VSCodium` for `Haskell` [template](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme).
It explains what's available in this project.
* Next, see Haskell [Prerequisites](https://github.com/br4ch1st0chr0n3/flakes/blob/main/README/Haskell.md).
* Following that, see [Troubleshooting](https://github.com/br4ch1st0chr0n3/flakes/blob/main/README/Troubleshooting.md).
* Recurse into `Prerequisites` on the linked pages to get even more info.

## Quick start

1. If you haven't yet started `VSCodium` provided by this flake:

    ```terminal
    nix develop
    write-settings-json
    codium .
    ```

1. Open a `Haskell` file `src/Main.hs` and hover over a function. `Haskell Language Server` should start giving you type info.

## Junior 1

[link](https://github.com/fullstack-development/developers-roadmap/blob/master/backend/junior-1#readme)

### Haskell

* [code](./src/Junior1.hs)

## Junior 3

[link](https://github.com/fullstack-development/developers-roadmap/tree/master/backend/junior-3#readme)

### Haskell

[code](./src/Junior3.hs)

#### Type classes

* The part before the `=>` is the context, while the part after the `=>` is the head of the instance declaration. - [src](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=overlapping%20instances#instance-declarations-and-resolution)

    ```hs
    instance (assertion1, ..., assertionn) => class type1 ... typem where ...
    ```

* How are type classes implemented in Haskell?
  * What is a dictionary?
    * A data type with class functions as fields
  * How is it defined and passed into functions? - [src](https://arxiv.org/pdf/1907.00844.pdf#subsection.2.1)
    * embed `Superclass` dictionary into `Subclass` dictionary

        ```hs
        data BaseD a = BaseD { base :: a -> Bool }
        data Sub1D a = Sub1D { 
              super1 :: BaseD a
            , sub1 :: a -> Bool 
        }
        ```

    * Passed automatically by the compiler
* Why using constraints on a type variable within a data declaration isn't a good idea?
  * They make code less flexible and disallow some instances - [SO](https://stackoverflow.com/a/40825913)
  * Can be achieved by using `GADTs`
* What is coherence and why is it important to maintain it? What are the possible cases of coherence violation?
  * > A program is coherent if it has exactly one meaning — i.e., its semantics is unambiguously determined.
  * `Coherence` is when multiple `type derivations` are possible - [SO](https://stackoverflow.com/a/68008592)
  * For each different derivation a different class instance can be used. This may lead to different behaviors
  * `FlexibleInstances` and `MultiParamTypeClasses` introduce incoherence
  * Need to maintain coherence to write a program whose type checking (`static`) doesn't change its runtime (`dynamic`) properties
* Overlapping
  * How does the instance selection process happen?
    * Find an instance with satisfying `B` of (`instance A => C B where`)
    * Find instance for `A`
  * Is it possible to have overlapping instances?
    * `instance C a` and `instance C Bool`
  * Does having overlapping instances violate coherence?
    * No
  * Basics of Haskell instance selection - [src](https://www.youtube.com/watch?v=XfIlhJFmw3c)
  * Is it possible to have a compiled and working program with coherence violations?
    * Yes - [src](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=overlapping%20instances#instance-signatures-type-signatures-in-instance-declarations) (see example above)
  * How would you solve a problem of overlapping instances in various situations?
    * Make the most specific instance discoverable using the fine-grained per-instance pragmas
    * Rewrite
      * `instance {-# OVERLAPPABLE #-} C a` and `instance C Bool`
      * `instance C a` and `instance {-# OVERLAPPING #-} C Bool`
      * `OVERLAPS` = both
* Orphans
  * What are orphan instances? Why are they undesirable?
    * An orphan instance is a type class instance for class C and type T which is neither defined in the module where C is defined nor in the module where T is defined. - [src](https://wiki.haskell.org/Orphan_instance)
    * Type class instances are special in that they don't have a name and cannot be `imported` explicitly. This also means that they cannot be `excluded` explicitly. All instances defined in a module `A` are imported automatically when importing `A`, or importing any module that imports `A`, directly or indirectly.
    * Orphans may break the functionality planned by the library author
    * Orphans invalidate file fingerprints (hash of a file made by GHC to check later if a file has changed) and transitively - in modules that import them - [src](https://tech.freckle.com/2018/12/12/a-home-for-orphan-instances/#fewer-dirty-fingerprints)
  * How to deliver orphans?
    * Expose type and instance only together by putting orphans into modules and re-exporting them - [src](https://www.michaelpj.com/blog/2020/10/29/your-orphans-are-fine.html#private-modules)
      * Cons:
        * a user has to use your instances
        * your lib uses more dependencies
    * Define instances in a separate package - [src](https://www.michaelpj.com/blog/2020/10/29/your-orphans-are-fine.html#private-packages)
      * cons: need to track these packages
  * Does having orphan instances violate coherence?
    * When orphans violate coherence:
      * If you actually import both instances, your program will fail to compile.
      * If you do not directly import both, but rather use two modules which independently use the differing instances, you can end up with incoherent behaviour.
  * What are the pros and cons of isolating orphans in special modules?
    * Pros: less often fingerprints invalidation
    * Cons: need to recompile the whole project on changes in that module - [src](https://tech.freckle.com/2018/12/12/a-home-for-orphan-instances/#decrease-the-surplus-compilation)
* How the problem of orphans and overlapping is solved in other languages or by different overloading implementation techniques?
  * Scala
    * An orphan instance in Scala means an instance that exists neither in the type’s companion object nor the type class’ companion object - [src](https://pjrt.medium.com/orphan-instances-in-scala-322caa78e382)
    * Import packages with type and instance declaration separately
* What are the problems of current typeclasses implementation?
  * There's no formal proof that instance resolution is coherent
* Is there a problem of structuring the hierarchy of standard typeclasses?
  <!-- TODO -->
* What is Final Tagless (FT) style? - [src](https://serokell.io/blog/introduction-tagless-final)
  * Example:
    * `wimble :: (MonadReader Env m, MonadState State m) => m ()`
  * Can extend in two dimensions
      1. a new interpreter (change implementation of `MonadReader`)
      2. a new set of operations (add a constraint like `MonadWriter`)
  * `Application monad` (`AM`) - a monad for organizing effectful application code
    * `FT` can define `AM`
  * `Tagged Initial` - sum types are represented as `(tag, payload)`. `tag` - for pattern-matching
  * `Tagless Initial` - use `GADTs` to ban nonsense expressions, no tags
  * `Final Tagless` - use overloaded functions

* `Functor` laws:

  ```hs
  fmap id = id
  ```

  ```hs
  fmap (f . g)  ==  fmap f . fmap g
  ```

### Type and Data Families

Haskell wiki ([src](https://wiki.haskell.org/GHC/Type_families#What_are_type_families.3F)):

> The concept of a type family comes from type theory. An indexed type family in type theory is a partial function at the type level. Applying the function to parameters (called type indices) yields a type. Type families permit a program to compute what data constructors it will operate on, rather than having them fixed statically (as with simple type systems) or treated as opaque unknowns (as with parametrically polymorphic types).

> Type families are to vanilla data types what type class methods are to regular functions. Vanilla polymorphic data types and functions have a single definition, which is used at all type instances. Classes and type families, on the other hand, have an interface definition and any number of instance definitions. A type family's interface definition declares its kind and its arity, or the number of type indices it takes. Instance definitions define the type family over some part of the domain.

* **Type Families: The Definitive Guide** - [src](https://serokell.io/blog/type-families-haskell)
  * `Generative` type
    > Note that we only need the notion of arity for type constructors that can reduce to other types when applied to an argument. For instance, `Pair Bool` is equal not only to itself but also to `(Bool, Bool)`.

    > On the other hand, `Maybe Bool` is only equal to itself: `Maybe Bool ~ Maybe Bool   -- reflexivity`

    > We thus call `Maybe` a generative type constructor, while `Pair` is non-generative.

    * Non-generative type constructors have arities assigned to them and must be used saturated.
  
  * Use to implement operations on GADTs

      ```hs
      type Append :: [a] -> [a] -> [a]
      type family Append xs ys where              -- header
        Append '[]    ys = ys                     -- clause 1
        Append (x:xs) ys = x : Append xs ys       -- clause 2
      happend :: HList xs -> HList ys -> HList (Append xs ys)
      ```
  
  * **Closed type families**
    * The clauses of a closed type family are ordered and matched **from top to bottom**
    * Overlapping equations

        ```hs
        type And :: Bool -> Bool -> Bool
        type family And a b where
          And True True = True
          And _    _    = False
        ```

  * **Open type families**
    * Such families can be extended anywhere
    * The equations of an open type family are either:
      * Not overlapping, so get a combinatorial explosion in patterns:

          ```hs
          type And' :: Bool -> Bool -> Bool
          type family And' a b

          type instance And' True  True  = True
          type instance And' True  False = False
          type instance And' False True  = False
          type instance And' False False = False
          ```

      * Compatible:
        * Can make right sides equal and unify left sides via rewriting

          ```hs
          type family G a b

          type instance G a Bool = a -> Bool
          type instance G Char b = Char -> b

          -- ==>

          type instance G Char Bool = Char -> Bool
          ```

  * **Associated types**
    * Almost the same as open type families
    * Can set default values

        ```hs
        type family Unwrap x where
          Unwrap (f a) = a

        class Container2 a where
          type Elem2 a
          -- default
          type Elem2 x = Unwrap x
          elements' :: a -> [Elem2 a]
        ```

    * Example from [string-interpolate](https://williamyaoh.com/posts/2019-05-27-string-interpolation-and-overlapping-instances.html#cb12)

  * **Injectivity** - get input types from output types
    * Use `TypeFamilyDependencies`

      ```hs
      type family Not x = r | r -> x where
      ghci> s @True
      "'False"
      ```

  * **Data families**
    * Compute **new** data types (type families compute the existing data types)

        ```hs
        data family Vector a
        newtype instance Vector () = VUnit Int
        newtype instance Vector Word8 = VBytes ByteArray
        ```

    * Can associate with a class

        ```hs
        class Vectorizable a where
        data Vector a
        vlength :: Vector a -> Int
        ```

### GADTs

* Wikibooks ([src](https://en.wikibooks.org/wiki/Haskell/GADT)):
  > With GADTs, a constructor for `Foo` a is not obliged to return `Foo a`; it can return any `Foo blah` that you can think of:

  ```hs
  data TrueGadtFoo a where
    MkTrueGadtFoo :: a -> TrueGadtFoo Int
  ```

  * Still, need to use a relevant data constructor

    ```hs
    data Foo where
      MkFoo :: Bar Int-- This will not typecheck
    ```

* Is it considered a good practice to put constraints in consructors inside GADT declaration?
  * No. Use a compiler to derive instances like `Functor`, put constraints in functions

* How can heterogenous lists be implemented with DataKinds and GADTs? - [src](https://www.parsonsmatt.org/2017/04/26/basic_type_level_programming_in_haskell.html#heterogeneous-lists)

  ```hs
  data HList xs where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

  infixr 6 :::

  hex :: HList '[Char, Integer, String]
  hex = 'a' ::: 1 ::: "hello" ::: HNil
  ```

### Kinds

* `DataKinds` - [src](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_data.html#type-level-data-declarations)
  * What is the data type promotion?
    * promote terms to type level like `'[1,2,3]` - [src]
* Are types with promoted kinds inhabited?
  * **inhabited types** (types that have at least 1 value) are of kind **Type**
* `ConstraintKinds` - `Constraint`s as first-class citizens

  ```hs
  type Stringy a = (Read a, Show a)
  ```

### Functional dependencies

* Problem ([src](https://www.fpcomplete.com/haskell/tutorial/fundeps/#exercises)):
  > we want a MonadReader typeclass where there is only a single instance per m, and we know the env parameter that will be available from each m.
  * Approach 1:
    * `MultiParamTypeClasses`) let us specify explicitly what the `env` is
    * `FunctionalDependencies` allow us to constrain ourselves to a single instance.

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

    * Approach 2:
      * `TypeFamilies`

          ```hs
          class MonadReader m where
            -- use an associated type
            type Env m
            ask :: m (Env m)
          
          -- `m (Env m)` calculates to `PersonReader Person`
          instance MonadReader PersonReader where
            type Env PersonReader = Person
            ask = PersonReader $ \env -> env
          ```

### Laziness

* Bang patterns

  ```hs
  {-# LANGUAGE BangPatterns #-}

  add :: Int -> Int -> Int
  add !x !y = x + y

  -- equivalent to
  add x y = x `seq` y `seq` x + y
  ```

* `Thunk` is an unevaluated expression - [src](https://stackoverflow.com/a/13984345)
  * `free variables` in an unevaluated expr
  * when evaluated, the pointers to it will point to the result
  * a `dead thunk` is `garbage collected`

* Expression forms - [src](https://stackoverflow.com/a/6889335)
  * Normal form
    > An expression in normal form is fully evaluated, and no sub-expression could be evaluated any further (i.e. it contains no un-evaluated thunks).
  * Weak head normal form
    > An expression in weak head normal form has been evaluated to the outermost data constructor or lambda abstraction (the head).

    ```hs
    (1 + 1, 2 + 2)       -- the outermost part is the data constructor (,)
    \x -> 2 + 2 
    ```

* `` a `seq` b `` - eval `a` to `WHNF`, return `b`
* `` a `deepseq` b `` - eval `a` to `NF`, return `b`
* `` force b = b `deepseq` b `` - eval `b` to `NF` and return `b`
  * If we have `let a = force b`, `a` is not in `NF`
  * To get `a` in `NF`, we need to `!a`
* `fix`

### File IO

* There are several representations of text in `Haskell` - `ByteString`, `Text`, `String`
* `ByteString` can contain both `human-readable` or `binary` data that mustn't be mixed
* Also, there are many `file encodings`. Use `UTF-8` to be safe
* One can encode standard data types into a `ByteString` using [Data.ByteString.Builder](https://hackage.haskell.org/package/bytestring-0.11.4.0/docs/Data-ByteString-Builder.html)
* `LBS` reads files in chunks. Can be used for streaming
* `hGet` reads a given number of bytes from a handle

### fused-effects

* [Defining new effects](https://github.com/fused-effects/fused-effects/blob/main/docs/defining_effects.md)
* [Reinterpreting effects](https://github.com/fused-effects/fused-effects/blob/main/docs/reinterpreting_effects.md)
* [Usage](https://github.com/fused-effects/fused-effects/blob/main/docs/usage.md)
* Any `monads`, including `mtl` transformer stacks, can be lifted via `Lift`
* Structure
  * `Effect` - define a data type and operations when  an effect in `sig`
  * `Carrier` - define type class instances, including `Algebra`
    * `Algebra` instance specifies how an effect’s constructors should be interpreted
    * `Carrier`s can handle more than one effect, and multiple carriers can be defined for the same effect, corresponding to different interpreters for the effect’s syntax.
  * Set whatever constraints necessary (e.g., `MonadIO`)
* `Algebra` instance examples:
  * [fused-effects-random](https://github.com/fused-effects/fused-effects-random/blob/fac39c49786158d6525ed185c94d64cca0ffcda3/src/Control/Carrier/Random/Gen.hs#L78)
  * [fused-effects-mwc-random](https://github.com/fused-effects/fused-effects-mwc-random/blob/d0d765e2842e980b9029a574693d99a622867ebb/src/Control/Carrier/Random/Lifted.hs#L41)
  * [fused-effects-parser](https://github.com/fused-effects/fused-effects-parser/blob/f18ea3cd59ac0983cbc100de42d9bace5b038d09/src/Control/Carrier/Parser/Church.hs#L190)
  * [fused-effects-readline](https://github.com/fused-effects/fused-effects-readline/blob/d8a35005ae28e827c92b4fe122f62d015a9a8e24/src/Control/Carrier/Readline/Haskeline.hs#L63)
  * [fused-effects-system](https://github.com/fused-effects/fused-effects-system/blob/b893c2955fa807087e945eff9f8bea4a9fd0be5c/fused-effects-time/src/Control/Carrier/Time/System.hs#L52)
  * [fused-effects-profile](https://github.com/fused-effects/fused-effects-system/blob/b893c2955fa807087e945eff9f8bea4a9fd0be5c/fused-effects-profile/src/Control/Carrier/Profile/Tree.hs#L48)
  * [fused-effects-time](https://github.com/fused-effects/fused-effects-system/blob/b893c2955fa807087e945eff9f8bea4a9fd0be5c/fused-effects-time/src/Control/Carrier/Time/System.hs#L52)
* Usage examples
  * [LearningHaskell](https://github.com/raymondtay/LearningHaskell/blob/master/src/Effects/fe/lib/Queries.hs)

### Debugging

* `Debug.Trace`
* `breakpoint` - [src](https://github.com/aaronallen8455/breakpoint)
  * put breakpoints into an app (see [TryBreakpoint](./app/TryBreakpoint.hs))
  * inspect variables visible at a breakpoint
  * freeze other threads (`GHC 9.2.x+`)

## Misc

* Parsing with Haskell
  * [Part 1](https://serokell.io/blog/lexing-with-alex)
* Haskell CI with caching - [src](https://github.com/aaronallen8455/breakpoint/blob/main/.github/workflows/ci.yml)
* `string-interpolate` - [src](https://hackage.haskell.org/package/string-interpolate)
  * UTF-8 string interpolation

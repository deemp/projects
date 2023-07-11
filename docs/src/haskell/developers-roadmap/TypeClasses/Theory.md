```haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Try.TypeClasses.Theory where
```

### Type classes

- The part before the `=>` is the context, while the part after the `=>` is the head of the instance declaration. - [src](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=overlapping%20instances#instance-declarations-and-resolution)

    ```hs
    instance (assertion1, ..., assertion) => class type1 ... typem where ...
    ```

- How are type classes implemented in Haskell?
  - [All You Wanted to Know About Type Classes](https://www.youtube.com/watch?v=8o51sA12VfI)
    - What is a dictionary?
      - A data type with class functions as fields
    - How is it defined and passed into functions? - [src](https://arxiv.org/pdf/1907.00844.pdf#subsection.2.1)
      - embed `Superclass` dictionary into `Subclass` dictionary

      <!-- i 6 -->

      ```haskell
      newtype BaseD a = BaseD {base :: a -> Bool}
      data Sub1D a = Sub1D
        { super1 :: BaseD a
        , sub1 :: a -> Bool
        }
      ```

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

#### Foldable

```hs
class Foldable t where
```

- When using folds, one can force the evaluation of an accumulator
  - `deepseq` - [YT](https://www.youtube.com/watch?v=eAcNGbnuTYs&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV&index=32)
  - [BangPatterns](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/strict.html) with pattern matching on the element of an accumulator to force.

    <!-- i 4 -->

    ```haskell
    -- >>> foldl (\(!a1, !a2) x -> (a1 + x, a2 + x)) (0, 0) [1..9]
    -- (45,45)
    ```

- `foldl'` - fold a list from the left: `f (f (f x a1) a2) ...` and have accumulator in WHNF.
  - May need to force the accumulator
- `foldr` - calculate the full list and fold it from the right: `f (f (f x a5) a4) ...`.
  - Can terminate early if an operation is strict in the left argument (like `&&`) - [SO](https://stackoverflow.com/a/27682341)
  - Can cause stack overflow as it has to evaluate the whole list first - [wiki](https://wiki.haskell.org/Foldr_Foldl_Foldl'#Foldr)

    ```haskell
    -- >>> foldr (&&) False (repeat False)
    -- False
    ```

- `fold :: (Foldable t, Monoid m) => t m -> m`
  - folds a container with elements that have a `Monoid` instance

    ```haskell
    -- >>> fold [Just "a", Nothing, Just "c"]
    -- Just "ac"
    ```

- `foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m` - maps each element of a container to a `Monoid` and `fold`s the container

    ```haskell
    -- >>> foldMap Just ["a", "b", "c"]
    -- Just "abc"
    ```

#### Alternative and MonadPlus

- [Haskell wikibooks](https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus):
  - `Alternative`
    - Definition

      ```hs
      class Applicative f => Alternative f where
        empty :: f a
        (<|>) :: f a -> f a -> f a
      ```

    - There's no instance for `Either a`
    - As it's an associative operation, it produces the same result for either fold

      <!-- i 6 -->

      ```haskell
      -- >>> foldr (<|>) empty [Just "a", Nothing, Just "c", Nothing, Just "e"]
      -- Just "a"
      
      -- >>> foldl' (<|>) empty [Just "a", Nothing, Just "c", Nothing, Just "e"]
      -- Just "a"
      ```

#### Traversable

- [Haskell wikibooks](https://en.wikibooks.org/wiki/Haskell/Traversable):

    <!-- i 4 -->

    ```haskell
    class (Functor t, Foldable t) => Traversable t where
      traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
      sequenceA :: (Applicative f) => t (f a) -> f (t a)
    
      -- These methods have default definitions.
      -- They are merely specialised versions of the other two.
      mapM :: (Monad m) => (a -> m b) -> t a -> m (t b)
      sequence :: (Monad m) => t (m a) -> m (t a)
    ```

#### Contravariant

- [Contravariant](https://github.com/ocharles/blog/blob/master/guest-posts/2013-12-21-24-days-of-hackage-contravariant.md)

#### Profunctor

- [Profunctor](https://github.com/ocharles/blog/blob/master/guest-posts/2013-12-22-24-days-of-hackage-profunctors.md)
- [Understanding profunctors](https://typeclasses.com/profunctors)

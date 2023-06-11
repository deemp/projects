{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Try.TypeFamilies.Theory where

import Data.Functor.Identity ( Identity )
import Data.Kind ( Type )

{-
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

{- i 6 -}

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

{-
  - **Closed type families**
    - The clauses of a closed type family are ordered and matched **from top to bottom**
    - Overlapping equations
-}

{- i 8 -}
type And :: Bool -> Bool -> Bool
type family And a b where
  And True True = True
  And _ _ = False

{-
  - **Open type families**
    - Such families can be extended anywhere
    - The equations of an open type family are either:
      - Not overlapping, so get a combinatorial explosion in patterns:
-}

{- i 8 -}

type And' :: Bool -> Bool -> Bool
type family And' a b

type instance And' True True = True
type instance And' True False = False
type instance And' False True = False
type instance And' False False = False

{-
      - Compatible:
        - Can make right sides equal and unify left sides via rewriting
-}

{- i 10 -}
type family G a b

type instance G a Bool = a -> Bool
type instance G Char b = Char -> b

-- ==>

type instance G Char Bool = Char -> Bool

{-
  - **Associated types**
    - Almost the same as open type families
    - Can set default values
-}

{- i 6 -}

type family Unwrap x where
  Unwrap (f a) = a

class Container2 a where
  type Elem2 a

  -- default
  type Elem2 x = Int
  elements' :: a -> [Elem2 a]

{-
    - Example from [string-interpolate](https://williamyaoh.com/posts/2019-05-27-string-interpolation-and-overlapping-instances.html#cb12)

  - **Injectivity** - get input types from output types
    - Use `TypeFamilyDependencies`
-}

{- i 8 -}
type family Not x = r | r -> x where

-- >>> s @True

{-
  - **Data families**
    - [HaskellWiki](https://wiki.haskell.org/GHC/Type_families#Detailed_definition_of_data_families)
    - Compute **new** data types (type families compute the existing data types)
-}

{- i 6 -}
data family Vector a
newtype instance Vector () = VUnit Int
newtype instance Vector Int = VInts [Int]

{-
    - Can associate with a class
-}
{- i 8 -}
class Vectorizable a where
  data Vector_ a
  vlength :: Vector_ a -> Int

newtype S = S {unS :: [Int]}
instance Vectorizable S where
  data Vector_ S = Vector_ {unVector_ :: S}
  vlength :: Vector_ S -> Int
  vlength = length . unS . unVector_

{-
# Optics by example

Notes on [Optics by example](https://leanpub.com/optics-by-example).

## Dev tools

### Prerequisites

See these for additional info:

- [codium-generic](https://github.com/deemp/flakes/tree/main/templates/codium/generic#readme) - info just about `VSCodium` and extensions.
- [codium-haskell](https://github.com/deemp/flakes/tree/main/templates/codium/haskell#readme) - an advanced version of this flake.
- [flake.nix](./flake.nix) - extensively commented code.
- [Haskell](https://github.com/deemp/flakes/blob/main/README/Haskell.md)
- [Troubleshooting](https://github.com/deemp/flakes/blob/main/README/Troubleshooting.md)
- [Prerequisites](https://github.com/deemp/flakes#prerequisites)

### Quick start

1. Install Nix - see [how](https://github.com/deemp/flakes/blob/main/README/InstallNix.md).

1. In a new terminal, run `VSCodium` from a devshell:

```console
nix flake new my-project -t github:deemp/flakes#codium-haskell-simple
cd my-project
git init && git add
nix develop
cabal run
```

1. Write `settings.json` and start `VSCodium`:

```console
nix run .#writeSettings
nix run .#codium .
```

#### Tools

#### GHC

This template uses `GHC 9.2`. You can switch to `GHC 9.0`:

- In `flake.nix`, change `"92"` to `"90"`

### Configs

- [package.yaml](./package.yaml) - used by `hpack` to generate a `.cabal`
- [.markdownlint.jsonc](./.markdownlint.jsonc) - for `markdownlint` from the extension `davidanson.vscode-markdownlint`
- [.ghcid](./.ghcid) - for [ghcid](https://github.com/ndmitchell/ghcid)
- [.envrc](./.envrc) - for [direnv](https://github.com/direnv/direnv)
- [fourmolu.yaml](./fourmolu.yaml) - for [fourmolu](https://github.com/fourmolu/fourmolu#configuration)
- [.github/workflows/ci.yaml] - a generated `GitHub Actions` workflow. See [workflows](https://github.com/deemp/flakes/tree/main/workflows). Generate a workflow via `nix run .#writeWorkflows`.
- `hie.yaml` - not present, but can be generated via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) (available on devshell) to verify the `Haskell Language Server` setup.

## Additional resources

- [lens ipynb](https://github.com/Elvecent/notebooks/blob/master/lens-aeson/Main.ipynb)
- [operators](https://github.com/ekmett/lens/wiki/Operators)
- [optics derivation](https://github.com/ekmett/lens/wiki/Derivation#traversals)

## Book
-}

{- FOURMOLU_DISABLE -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- LIMA_DISABLE -}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{- LIMA_ENABLE -}
{- FOURMOLU_ENABLE -}

module Main (main) where

import Control.Lens
import Control.Lens.Unsound (lensProduct)
import Data.ByteString qualified as BS
import Data.Char (toUpper)
import Data.Foldable (Foldable (..))
import Data.Map qualified as M
import Data.Monoid (Sum (..))
import Data.Ord (comparing)
import Data.Text qualified as T
import GHC.Word qualified

main :: IO ()
main = print "hello"

{-
## 3. Lenses

- A Lens must focus ONE thing inside a structure.
- A Lens must never fail to get or set that focus.

### 3.1 Introduction to Lenses

#### Exercises - Optic Anatomy

Find: action, path, structure, focus
-}

-- This will be evaluated by HLS
-- >>> view (_1 . _2) ((1, 2), 3)
-- 2

-- This will be evaluated by ghcid

-- $> print "Hello"

{-
- action: 'view'
- path: `(_1 . _2)`
- structure: `((1, 2), 3)`
- focus: `2`
-}

-- >>> set (_2 . _Left) "new" (False, Left "old")
-- (False,Left "new")

{-
- action: `set`
- path: `(_2 . _Left)`
- structure: `(False, Left "old")`
- focus: `"old"`
-}

-- >>> over (taking 2 worded . traversed) toUpper "testing one two three"
-- "TESTING ONE two three"

{-
- action: `over`
- path: `(taking 2 worded . traversed)`
- structure: `"testing one two three"`
- focus: `"testing one"`
-}

-- >>>foldOf (both . each) (["super", "cali"],["fragilistic", "expialidocious"])
-- "supercalifragilisticexpialidocious"

{-
- action: `foldOf`
- path: `(both . each)`
- structure: `(["super", "cali"],["fragilistic", "expialidocious"])`
- focus: `["super", "cali", "fragilistic", "expilidocious"]`
-}

{-
### 3.2 Lens Actions
-}

-- >>>view _1 ('a', 'b')
-- 'a'

-- >>> set _1 'x' ('a', 'b')
-- ('x','b')

-- >>> over _1 (*100) (1, 2)
-- (100,2)

{-
#### Exercises - Lens Actions
-}

{- 2. solution: -}

{- LIMA_INDENT 4 -}

ex1 :: Lens' (Char, Int) Char
ex1 = undefined

{-
3. Lens actions:

    - get
    - set
    - modify
-}

{- 4. focus on `c` -}

-- >>>view _3 ('a','b','c')
-- 'c'

-- >>>s = over _2 (*10) (False, 2)
-- >>>:t s
-- s :: Num b => (Bool, b)
-- >>>s
-- (False,20)

{- LIMA_DEDENT -}

{-
### 3.3 Lenses and records
-}

data Ship = Ship {_name :: String, _numCrew :: Int} deriving (Show)

name_ :: Lens' Ship String
name_ = lens getName setName
 where
  getName :: Ship -> String
  getName = _name
  setName :: Ship -> String -> Ship
  setName ship _name = ship{_name}

purplePearl :: Ship
purplePearl = Ship{_name = "Purple Pearl", _numCrew = 38}

{- 1. apply lens -}

{- LIMA_INDENT 4 -}

-- >>>view name_ purplePearl
-- "Purple Pearl"

-- >>>over name_ (const "Purple  Pearl") purplePearl
-- Ship {_name = "Purple  Pearl", _numCrew = 38}

makeLenses ''Ship

-- >>>:t name
-- name :: Lens' Ship String

{- LIMA_DEDENT -}

{-
#### Exercises - Records Part Two
-}

{- 2. Rewrite -}

{- LIMA_INDENT 4 -}
data Spuzz
data Chumble
gazork :: Functor f => (Spuzz -> f Spuzz) -> Chumble -> f Chumble
gazork = undefined

gazork_ :: Lens' Spuzz Chumble
gazork_ = undefined

{- LIMA_DEDENT -}

{-
## 3.4 Limitations

 **Lens** - An optic which always accesses **exactly one focus**.

### Exercises

1. Can make both a getter and a setter
-}

{- LIMA_INDENT 4 -}
get1 :: (a, b, c) -> b
get1 (_, b, _) = b

set1 :: (a, b, c) -> b -> (a, b, c)
set1 (a, _, c) b_ = (a, b_, c)

{- 2. Can't get from `Nothing`, so, can't have `inMaybe :: Lens' (Maybe a) a` not fail sometimes -}
get2 :: Maybe a -> a
get2 (Just a) = a
get2 _ = undefined

{-
3. Similar situation with `left :: Lens' (Either a b) a`

4. No, a list may have < 2 elements

5. Yes, you always can set and get a value, and there'll be only one value focused
-}

conditional :: Lens' (Bool, a, a) a
conditional = undefined

{- LIMA_DEDENT -}

{-
## 3.5 Lens Laws

Allow to reason about a lens' behavior.

1. You get back what you set (set-get)
    - `view myLens (set myLens newValue structure) == newValue`
1. Setting back what you got doesn't do anything (get-set)
    - `set myLens (view myLens structure) structure == structure`
1. Setting twice is the same as setting once (set-set)
    - `set myLens differentValue (set myLens value structure) == set myLens differentValue structure`

### Unlawful lenses

When using unlawful lenses in a library, should write a note.

`lensProduct` combines two lenses to get a new one

- these lenses should be **disjoint**. Otherwise, how to set?

-}
newtype Ex1 = Ex1 {_unEx1 :: String} deriving (Show, Eq)

makeLenses ''Ex1

alongsideEx1 :: Lens' Ex1 (Ex1, String)
alongsideEx1 = lensProduct id unEx1

ex3 :: Ex1
ex3 = Ex1 "c"

ex4 :: (Ex1, String)
ex4 = (Ex1 "a", "b")

-- ex5 :: Bool
ex5 :: (Ex1, String)
ex5 = view alongsideEx1 (set alongsideEx1 ex4 ex3)

{-
We don't get back what we set:
-}

-- >>>ex5
-- (Ex1 {_unEx1 = "b"},"b")

-- >>>ex4 == ex5
-- False

{-
#### Exercises - Laws

1. break `get-set`
-}

{- LIMA_INDENT 4 -}

break2 :: Lens' Ex1 String
break2 = lens (const "1") (\_ _ -> Ex1 "2")

ex6 :: String
ex6 = view break2 ex3

-- >>>ex6
-- "1"

ex7 :: Ex1
ex7 = set break2 ex6 ex3

-- >>>ex7
-- Ex1 {_unEx1 = "2"}

{-
2. `get-set`, `set-set` work, `set-get` fails
-}

data Err
  = ReallyBadError {_msg :: String}
  | ExitCode {_code :: Int}
  deriving (Show, Eq)

msg :: Lens' Err String
msg = lens getMsg setMsg
 where
  getMsg (ReallyBadError message) = message
  -- Hrmm, I guess we just return ""?
  getMsg (ExitCode _) = ""
  setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
  -- Nowhere to set it, I guess we do nothing?
  setMsg (ExitCode n) _ = ExitCode n

err :: Err
err = ExitCode 3

msgTest :: Bool
msgTest =
  view msg (set msg "a" err) /= "a"
    && set msg (view msg err) err == err
    && set msg "a" (set msg "a" err) == set msg "a" err

-- >>>msgTest
-- True

{- 3. fail `get-set`, pass other -}

msg1 :: Lens' Err String
msg1 = lens getMsg setMsg
 where
  getMsg (ReallyBadError message) = message
  -- Hrmm, I guess we just return ""?
  getMsg (ExitCode _) = ""
  setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
  -- Nowhere to set it, I guess we do nothing?
  setMsg (ExitCode _) x = ReallyBadError x

msg1Test :: Bool
msg1Test =
  set msg1 (view msg1 err) err /= err
    && set msg1 "a" (set msg1 "a" err) == set msg1 "a" err
    && view msg1 (set msg1 "a" err) == "a"

-- >>>msg1Test
-- True

{- 4. like `msg1` -}

data Sink = A Int | B String deriving (Show, Eq)

sink :: Lens' Sink String
sink = lens getSink setSink
 where
  getSink (A x) = show x
  getSink (B x) = x
  setSink (A _) x = B x
  setSink (B _) x = B x

sinkEx :: Sink
sinkEx = A 4

sinkTest :: Bool
sinkTest =
  set sink (view sink sinkEx) sinkEx /= sinkEx
    && view sink (set sink "a" sinkEx) == "a"
    && set sink "a" (set sink "a" sinkEx) == set sink "a" sinkEx

-- >>>sinkTest
-- True

{- 5. break all rules -}

newtype Break = Break String deriving (Show, Eq)

break_ :: Break
break_ = Break "hey"

breakAll :: Lens' Break String
breakAll = lens get_ set_
 where
  get_ (Break _) = "!"
  set_ (Break s) x = Break $ s ++ x

breakAllTest :: Bool
breakAllTest =
  set breakAll (view breakAll break_) break_ /= break_
    && view breakAll (set breakAll "a" break_) /= "a"
    && set breakAll "a" (set breakAll "a" break_) /= set breakAll "a" break_

-- >>>breakAllTest
-- True

{- 6. builder -}

data Builder = Builder
  { _context :: [String]
  , _build :: [String] -> String
  }

instance Eq Builder where
  (==) :: Builder -> Builder -> Bool
  x == y = x._context == y._context

builderLens :: Lens' Builder String
builderLens = lens builderGet builderSet
 where
  builderGet (Builder{..}) = case _context of [] -> ""; s -> head s
  builderSet (Builder{..}) s = Builder{_context = case s of "" -> []; _ -> [s], ..}

builder1 :: Builder
builder1 = Builder{_context = [], _build = fold}

builderTest :: Bool
builderTest =
  set builderLens (view builderLens builder1) builder1 == builder1
    && view builderLens (set builderLens "a" builder1) == "a"
    && view builderLens (set builderLens "" builder1) == ""
    && set builderLens "a" (set builderLens "a" builder1) == set builderLens "a" builder1
    && set builderLens "" (set builderLens "" builder1) == set builderLens "" builder1

-- >>>builderTest
-- True

{- LIMA_DEDENT -}

{-
### 3.6 Virtual Fields

Export only lenses, not constructors. This is to make importing modules independent of a type's inner representation.

For a data type, we can make lenses that hide some computations on the existing type's fields and lenses.
-}

data Temperature = Temperature
  { _location :: String
  , _celsius :: Float
  }
  deriving (Show)

makeLenses ''Temperature

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9 / 5)) + 32
fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5 / 9)

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
 where
  getter = celsiusToFahrenheit . view celsius
  setter temp_ f = set celsius (fahrenheitToCelsius f) temp_

temp :: Temperature
temp = Temperature "Berlin" 7.0

-- >>>over fahrenheit (+18) temp
-- Temperature {_location = "Berlin", _celsius = 17.0}

{-
When changing a field's name in the original data type,
we can separately export a lens for the old field.
This lens is calculated based on the updated type's fields and lenses.
-}

data Temperature_ = Temperature_
  { _location_ :: String
  , _kelvin_ :: Float
  }
  deriving (Show)

makeLenses ''Temperature_

celsius_ :: Lens' Temperature_ Float
celsius_ = lens getter setter
 where
  getter = subtract 273.15 . view kelvin_
  setter temp_ c = set kelvin_ (c + 273.15) temp_

{-
#### Exercises - Virtual Fields

1. substitute lens
-}

{- LIMA_INDENT 4 -}

data User = User
  { _firstName :: String
  , _lastName :: String
  , _email :: String
  }
  deriving (Show)

makeLenses ''User

username :: Lens' User String
username = lens getter setter
 where
  getter = view email
  setter user_ s = set email s user_

{-
2. unlawful `fullName` lens
-}

fullName :: Lens' User String
fullName = lens getter setter
 where
  getter user_ = view firstName user_ ++ " " ++ view lastName user_
  setter user_ f =
    let fname : (unwords -> lname) = words f
     in set firstName fname (set lastName lname user_)

user :: User
user = User "John" "Cena" "invisible@example.com"

-- >>>view fullName user
-- "John Cena"

-- >>>set fullName "Doctor of Thuganomics" user
-- User {_firstName = "Doctor", _lastName = "of Thuganomics", _email = "invisible@example.com"}

{- LIMA_DEDENT -}

{-
### 3.7  Data correction and maintaining invariants

We can provide some advanced logic in our setters and getters.
E.g., saturate a number to a value between a pair of given values.

#### Exercises - Self-Correcting Lenses

1. and 2.
-}

{- LIMA_INDENT 4 -}

data ProducePrices = ProducePrices
  { _limePrice :: Float
  , _lemonPrice :: Float
  }
  deriving (Show)

limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
 where
  getter = _limePrice
  setter ProducePrices{..} p =
    ProducePrices
      { _limePrice = newLimePrice
      , _lemonPrice =
          if abs (_lemonPrice - newLimePrice) <= 0.5
            then _lemonPrice
            else max (newLimePrice + signum (_lemonPrice - newLimePrice) * 0.5) 0
      }
   where
    newLimePrice = max p 0

prices :: ProducePrices
prices = ProducePrices 1.50 1.48

-- >>>set limePrice 2 prices
-- ProducePrices {_limePrice = 2.0, _lemonPrice = 1.5}

-- >>>set limePrice 1.8 prices
-- ProducePrices {_limePrice = 1.8, _lemonPrice = 1.48}

-- >>> set limePrice 1.63 prices
-- ProducePrices {_limePrice = 1.63, _lemonPrice = 1.48}

-- >>>  set limePrice (-1.00) prices
-- ProducePrices {_limePrice = 0.0, _lemonPrice = 0.5}

{- LIMA_DEDENT -}

{-
## 4 Polymorphic Optics

```hs
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
```

- `s`: structure before action
- `t`: structure after action
- `a`: focus before action
- `b`: focus after action

<b> We need polymorphic lenses whenever an action might want to change the type of the focus. </b>
-}

ex8 :: ([Char], Int)
ex8 = over _1 show (1 :: Int, 1)

-- >>>ex8
-- ("1",1)

data Promotion a = Promotion
  { _item :: a
  , _discountPercentage :: Double
  }
  deriving (Show)

{-
### 4.2 When do we need polymorphic lenses

`over :: Lens' s a -> (a -> a) -> s -> s`

#### Changing type variables with polymorphic lenses
-}

item :: Lens (a, b) (c, b) a c
item = lens getter setter
 where
  getter :: (a, b) -> a
  getter = fst
  setter :: (a, b) -> c -> (c, b)
  setter (_, b) c = (c, b)

{-
#### Exercises – Polymorphic Lenses
-}

{- 1. `Vorpal` -}

{- LIMA_INDENT 4 -}
data Vorpal a

vorpal :: Lens (Vorpal a) (Vorpal b) a b
vorpal = undefined

{- 2. Polymorphic unlawful -}

data Preferences a = Preferences {_best :: a, _worst :: a} deriving (Show)

best :: Lens (Preferences a) (Preferences b) a b
best = lens getter setter
 where
  getter (Preferences a _) = a
  setter (Preferences _ _) c = Preferences{_best = c, _worst = c}

{- 3. Result -}

data Result e = Result {_lineNumber :: Int, _result :: Either e String}

result :: Lens (Result a) (Result b) a b
result = undefined

{- 4. Multiple -}

data Multi a b

multi :: Lens (Multi a b) (Multi c d) (a, b) (c, d)
multi = undefined

{- 5. Predicate -}

newtype Predicate a = Predicate (a -> Bool)

predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicate = lens getter setter
 where
  getter (Predicate x) = x
  setter (Predicate _) = Predicate

{- LIMA_DEDENT -}

{-
#### How do Lens Types Compose?

We compose `Lens' a b` and `Lens' b c`.

Inside, they are `b -> a` and `c -> b` so that we can compose them like `(b -> a) . (c -> b)`
-}

ex9 :: forall a b c d e f. (e -> f)
ex9 = (d . s) m
 where
  m :: a -> b
  m = undefined
  s :: (a -> b) -> (c -> d)
  s = undefined
  d :: (c -> d) -> (e -> f)
  d = undefined

{-
##### Example
-}
data Person
data Address
data StreetAddress

personAddressLens :: forall f. Functor f => (Address -> f Address) -> Person -> f Person
personAddressLens = undefined

personAddressLens_ :: Lens Person Person Address Address
personAddressLens_ = undefined

addressStreetLens :: forall f. Functor f => (StreetAddress -> f StreetAddress) -> Address -> f Address
addressStreetLens = undefined

addressStreetLens_ :: Lens Address Address StreetAddress StreetAddress
addressStreetLens_ = undefined

personStreetLens :: Functor f => (StreetAddress -> f StreetAddress) -> Person -> f Person
personStreetLens = personAddressLens . addressStreetLens

personStreet :: StreetAddress
personStreet = view personStreetLens (undefined :: Person)

{-
#### Exercises – Lens Composition

1. Pairs
-}

{- LIMA_INDENT 4 -}
-- >>> view (_2 . _1 . _2) ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))
-- "Waldo"

{- 2. Domino -}

data Five
data Eight
data Two
data Three

fiveEightDomino :: Lens' Five Eight
fiveEightDomino = undefined
twoThreeDomino :: Lens' Two Three
twoThreeDomino = undefined
dominoTrain :: Lens' Five Three
dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino

mysteryDomino :: Lens' Eight Two
mysteryDomino = undefined

{- 3. Rewrite -}

data Armadillo
data Hedgehog
data Platypus
data BabySloth

g :: Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
g = undefined

h :: Lens Platypus BabySloth Armadillo Hedgehog
h = undefined

{- 4. Compose -}

data Gazork
data Trowlg
data Bandersnatch
data Yakka
data Zink
data Wattoom
data Grug
data Pubbawup
data Foob
data Mog
data Boojum
data Jabberwock
data Snark
data JubJub

snajubjumwock :: Lens Snark JubJub Boojum Jabberwock
snajubjumwock = undefined
boowockugwup :: Lens Boojum Jabberwock Grug Pubbawup
boowockugwup = undefined
gruggazinkoom :: Lens Grug Pubbawup Zink Wattoom
gruggazinkoom = undefined
zinkattumblezz :: Lens Zink Wattoom Chumble Spuzz
zinkattumblezz = undefined
spuzorktrowmble :: Lens Chumble Spuzz Gazork Trowlg
spuzorktrowmble = undefined
gazorlglesnatchka :: Lens Gazork Trowlg Bandersnatch Yakka
gazorlglesnatchka = undefined
banderyakoobog :: Lens Bandersnatch Yakka Foob Mog
banderyakoobog = undefined

ex10 :: (Foob -> [Mog]) -> Snark -> [JubJub]
ex10 = snajubjumwock @[] . boowockugwup . gruggazinkoom . zinkattumblezz . spuzorktrowmble . gazorlglesnatchka . banderyakoobog

{- LIMA_DEDENT-}

{-
## 5. Operators

<b>Fixity</b> - operator precedence
-}

-- >>>:t _1 . _2 .~ 3
-- _1 . _2 .~ 3 :: (Field1 s t a1 b1, Field2 a1 b1 a2 b2, Num b2) => s -> t

{- is equivalent to -}

-- >>>:t (_1 . _2) .~ 3
-- (_1 . _2) .~ 3 :: (Field1 s t a1 b1, Field2 a1 b1 a2 b2, Num b2) => s -> t

{-
We can use `&` to make a convenient-to-read chain
-}

-- >>>((2,3),4) & (_1 . _2) .~ 5
-- ((2,5),4)

-- >>> :{
-- unknown command '{'
multiline :: Integer
multiline = 3

{- Or even -}

ex11 :: ((Integer, Integer), (Integer, Integer))
ex11 =
  ((2, 3), (4, 6))
    & (_1 . _2) .~ 5
    & (_2 . _1) .~ 5

-- >>>ex9
-- ((2,5),(5,6))

{-
Optics operators - [src](https://github.com/Zelenya/chrome-annotation-extension-optics/blob/6d9d4459fefc80b36b5e2fc2271fbaaee2923911/src/content.js#L11-L138)

- `<|` `cons`
- `|>` `snoc`
- `^..` `toListOf`
- `^?` `preview`/`head`
- `^?!` **UNSAFE** `preview`/`head`
- `^@..` `itoListOf`
- `^@?` **SAFE** `head` (with index)
- `^@?!` **UNSAFE** `head` (with index)
- `^.` `view`
- `^@.` `iview`
- `<.` a function composition (`Indexed` with non-indexed)
- `.>` a function composition (non-indexed with `Indexed`)
- `<.>` a composition of Indexed functions
- `%%~` modify target; extract functorial/applicative result
- `%%=` modify target in state; return extra information
- `&~` used to chain lens operations
- `<&>` a flipped version of `<$>`
- `??` used to flip argument order of composite functions
- `<%~` `modify` lens target; return result
- `<+~` increment lens target; return result
- `<-~` decrement lens target; return result
- `<*~` multiply lens target; return result
- `<//~` divide lens target; return result
- `<^~` raise lens target; return result
- `<^^~` raise lens target; return result
- `<**~` raise lens target; return result
- `<||~` logically-or lens target; return result
- `<&&~` logically-and lens target; return result
- `<<%~` `modify` lens target, return old value
- `<<.~` replace lens target, return old value
- `<<?~` replace lens target (with `Just value`), return old value
- `<<+~` increment lens target; return old value
- `<<-~` decrement lens target; return old value
- `<<*~` multiply lens target; return old value
- `<<//~` divide lens target; return old value
- `<<^~` raise lens target; return old value
- `<<^^~` raise lens target; return old value
- `<<**~` raise lens target; return old value
- `<||~` logically-or lens target; return old value
- `<&&~` logically-and lens target; return old value
- `<<<>~` `modify` lens target with (`<>`); return old value
- `<%=` `modify` target in state; return result
- `<+=` add to target in state; return result
- `<-=` subtract from target in state; return result
- `<*=` multiple the target in state; return result
- `<//=` divide the target in state; return result
- `<^=` raise lens target in state; return result
- `<^^=` raise lens target in state; return result
- `<**=` raise lens target in state; return result
- `<||=` logically-or lens target in state; return result
- `<&&=` logically-and lens target in state; return result
- `<<%=` `modify` lens target in state; return old value
- `<<.=` replace lens target in state; return old value
- `<<?=` replace target (with Just value) in state, return old value
- `<<+=` add to target in state; return old value
- `<<-=` subtract from target in state; return old value
- `<<*=` multiple the target in state; return old value
- `<<//=` divide the target in state; return old value
- `<<^=` raise lens target in state; return old value
- `<<^^=` raise lens target in state; return old value
- `<<**=` raise lens target in state; return old value
- `<<||=` logically-or lens target in state; return old value
- `<<&&=` logically-and lens target in state; return old value
- `<<<>=` `modify` target with (`<>`) in state; return old value
- `<<~` run monadic action, set lens target
- `<<>~` (`<>`) onto the end of lens target; return result
- `<<>=` (`<>`) onto the end of lens target in state; return result
- `<%@~` `modify` `IndexedLens` target; return intermediate result
- `<<%@~` modify `IndexedLens` target; return old value
- `%%@~` modify `IndexedLens` target; return supplementary result
- `%%@=` modify `IndexedLens` target in state; return supplementary result
- `<%@=` modify `IndexedLens` target in state; return intermediate result
- `<<%@=` modify `IndexedLens` target in state; return old value
- `^#` `view` (`ALens` version)
- `#~` `set` (`ALens` version)
- `#%~` `over` (`ALens` version)
- `#%%~` `modify` `ALens` target; extract functorial/applicative result
- `%%=` `modify` target in state; return extra information
- `#=` `assign` (`ALens` version)
- `#%=` `map` over `ALens` target(s) in state
- `<#%~` `modify` `ALens` target; return result
- `<#%=` `modify` `ALens` target in state; return result
- `#%%=` `modify` `ALens` target in state; return extra information
- `<#~` `set` with pass-through (`ALens` version)
- `<#=` `set` with pass-through in state (`ALens` version)
- `%~` `over` / `modify` target(s)
- `.~` `set`
- `?~` `set` to `Just value`
- `<.~` `set` with pass-through
- `<?~` `set` to `Just value` with pass-through
- `+~` increment target(s)
- `*~` multiply target(s)
- `-~` decrement target(s)
- `//~` divide target(s)
- `^~` raise target(s)
- `^~` raise target(s)
- `^^~` raise target(s)
- `**~` raise target(s)
- `||~` logically-or target(s)
- `&&~` logically-and target(s)
- `.=` assign in state
- `%=` map over target(s) in state
- `?=` `set` target(s) to `Just value` in state
- `+=` add to target(s) in state
- `*=` multiply target(s) in state
- `-=` decrement from target(s) in state
- `//=` divide target(s) in state
- `^=` raise target(s) in state
- `^=` raise target(s) in state
- `^^=` raise target(s) in state
- `**=` raise target(s) in state
- `||=` logically-or target(s) in state
- `&&=` logically-and target(s) in state
- `<~` run monadic action, `set` target(s) in state
- `<.=` `set` with pass-through in state
- `<?=` `set` `Just value` with pass-through in state
- `<>~` `modify` target with (`<>`)
- `<>=` `modify` target with (`<>`) in state
- `.@~` `iset` / set target(s) with index
- `.@=` set target(s) in state with index
- `%@~` `iover` / `modify` target(s) with index
- `%@=` `modify` target(s) in state with index
- `&` a reverse application operator
- `#` review
- `id` focus the `full` structure
-}

{-
### 5.9 Exercises – Operators
-}

{-
1. Get to
-}

{- LIMA_INDENT 4 -}

data Gate = Gate {_open :: Bool, _oilTemp :: Float} deriving (Show)
makeLenses ''Gate
data Army = Army {_archers :: Int, _knights :: Int} deriving (Show)
makeLenses ''Army
data Kingdom = Kingdom {_name1 :: String, _army :: Army, _gate :: Gate} deriving (Show)
makeLenses ''Kingdom
duloc :: Kingdom
duloc = Kingdom{_name1 = "Duloc", _army = Army{_archers = 22, _knights = 14}, _gate = Gate{_open = True, _oilTemp = 10.0}}

goalA :: Kingdom
goalA = duloc & name1 <>~ ": a perfect place" & army . knights *~ 3 & gate . open &&~ False

-- >>>goalA
-- Kingdom {_name1 = "Duloc: a perfect place", _army = Army {_archers = 22, _knights = 42}, _gate = Gate {_open = False, _oilTemp = 10.0}}

goalB :: Kingdom
goalB = duloc & name1 <>~ "cinstein" & army . archers -~ 5 & army . knights +~ 12 & gate . oilTemp *~ 10

-- >>>goalB
-- Kingdom {_name1 = "Duloccinstein", _army = Army {_archers = 17, _knights = 26}, _gate = Gate {_open = True, _oilTemp = 100.0}}

goalB_ :: Kingdom
goalB_ = duloc & name1 <>~ "cinstein" & army %~ (\x -> x & archers -~ 5 & knights +~ 12) & gate . oilTemp *~ 10

-- >>>goalB_
-- Kingdom {_name1 = "Duloccinstein", _army = Army {_archers = 17, _knights = 26}, _gate = Gate {_open = True, _oilTemp = 100.0}}

goalC :: (String, Kingdom)
goalC = duloc & gate . oilTemp //~ 2 & name1 <>~ ": Home" & name1 <<%~ (<> "of the talking Donkeys")

-- >>>goalC
-- ("Duloc: Home",Kingdom {_name1 = "Duloc: Homeof the talking Donkeys", _army = Army {_archers = 22, _knights = 14}, _gate = Gate {_open = True, _oilTemp = 5.0}})

{-
2. Enter code
-}

ex12 :: (Bool, [Char])
ex12 = (False, "opossums") & _1 ||~ True

-- >>>ex10
-- (True,"opossums")

ex13 :: Integer
ex13 = 2 & id *~ 3

-- >>>ex11
-- 6

ex14 :: ((Bool, [Char]), Double)
ex14 =
  ((True, "Dudley"), 55.0)
    & (_1 . _2 <>~ " - the worst")
    & (_2 -~ 15)
    & (_2 //~ 2)
    & (_1 . _2 %~ map toUpper)
    & (_1 . _1 .~ False)

-- >>>ex12
-- ((False,"DUDLEY - THE WORST"),20.0)

{-
3. `&`
-}

{-
4. `(%~) :: Lens s t a b -> (a -> b) -> s -> t`
-}

{- LIMA_DEDENT -}

{-
## 6. Folds

- have no laws!
- focus on several elements
- composition makes successive folds focus on the elements of previous focuses, forming a tree
- the result of a composite fold is a `Foldable` of leaves of such a tree
- combinators can work with a set of focuses (leaves) at a necessary level of such a tree
-}

-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded . taking 2 folded
-- [1,2,10,20,100,200]

{-
### 6.1 Introduction to Folds

- Folds can focus **MANY** things, Lenses must focus **ONE** thing
- Folds can only **get** zero or more things, Lenses must always be able to **get** and **set**
- Folds aren't polymorphic

#### Focusing all elements of a container

```hs
type Fold s a = forall m. Monoid m => Getting m s a
type Getting r s a = (a -> Const r a) -> s -> Const r s
newtype Const a (b :: k) = Const { getConst :: a }
```

- `s`: structure
- `a`: focus

#### Collapsing the Set

```hs
folded :: Foldable f => Fold (f a) a
```
-}

ex15 :: [Integer]
ex15 = [Just 3, Nothing, Nothing] ^.. folded . _Just

-- >>>ex15
-- [3]

{-
#### Using lenses as folds

We have

```hs
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Fold s a = forall m. Monoid m => Getting m s a
type Getting r s a = (a -> Const r a) -> s -> Const r s
```

So, we can use a `Lens' s a` as a `Fold s a`

- `^..` first applies the folds, and returns them in a list

-}

getPair2 :: Fold (a, b) b
getPair2 = _2

-- >>>(3,4) ^.. getPair2
-- [4]

{-
#### Foundational fold combinators

- `both` - Traverse both parts of a Bitraversable container with matching types
- `each` - generalizes `both` for tuples
-}

ex16 :: [Integer]
ex16 = (1, 2) ^.. both

-- >>>ex16
-- [1,2]

ex17 :: [Integer]
ex17 = (1, 2, 4, 5, 6) ^.. each

-- >>>ex17
-- [1,2,4,5,6]

ex18 :: [GHC.Word.Word8]
ex18 = ("Do or do not" :: BS.ByteString) ^.. each

{-
#### Exercises – Simple Folds

1. beasts
-}

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

-- >>> beastSizes ^.. folded
-- [(3,"Sirens"),(882,"Kraken"),(92,"Ogopogo")]

-- >>> beastSizes ^.. folded . folded
-- ["Sirens","Kraken","Ogopogo"]

-- >>> beastSizes ^.. folded . folded . folded
-- "SirensKrakenOgopogo"

-- >>> beastSizes ^.. folded . _2
-- ["Sirens","Kraken","Ogopogo"]

-- >>> toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]
-- [1,2,3,4,5,6]

ex19 :: [Char]
ex19 = toListOf (folded . folded @[]) (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])

-- >>> ex19
-- "CaptainFirst Mate"

-- >>> ("Hello" :: String, "It's me") ^.. both . folded
-- "HelloIt's me"

-- >>> ("Why", "So", "Serious?") ^.. each
-- ["Why","So","Serious?"]

quotes :: [(T.Text, T.Text, T.Text)]
quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

ex20 :: [Char]
ex20 = quotes ^.. each . each . each

-- >>> ex20
-- "WhySoSerious?ThisisSPARTA"

{-
1. Blank
-}

-- >>>[1, 2, 3] ^.. folded
-- [1,2,3]

-- >>> ("Light", "Dark") ^.. _1
-- ["Light"]

-- >>> [("Light", "Dark"), ("Happy", "Sad")] ^.. each . each
-- ["Light","Dark","Happy","Sad"]

-- >>> [("Light", "Dark"), ("Happy", "Sad")] ^.. each . _1
-- ["Light","Happy"]

ex21 :: String
ex21 = ([("Light", "Dark" :: String), ("Happy", "Sad")] ^.. each . _2) ^.. each . each

-- >>> ex21
-- "DarkSad"

-- >>> ("Bond", "James", "Bond") ^.. each
-- ["Bond","James","Bond"]

{-
### 6.2 Custom Folds

We should project the pieces of a structure into something `Foldable`. Then, we can construct a `Fold`.

```hs
folding :: Foldable f => (s -> f a) -> Fold s a
```
-}

newtype Name = Name
  { getName :: String
  }
  deriving (Show)
data ShipCrew = ShipCrew
  { _shipName :: Name
  , _captain :: Name
  , _firstMate :: Name
  , _conscripts :: [Name]
  }
  deriving (Show)
makeLenses ''ShipCrew

myCrew :: ShipCrew
myCrew =
  ShipCrew
    { _shipName = Name "Purple Pearl"
    , _captain = Name "Grumpy Roger"
    , _firstMate = Name "Long-John Bronze"
    , _conscripts = [Name "One-eyed Jack", Name "Filthy Frank"]
    }

collectCrewMembers :: ShipCrew -> [Name]
collectCrewMembers sc = [sc ^. captain, sc ^. firstMate] ++ sc ^. conscripts

crewMembers :: Fold ShipCrew Name
crewMembers = folding collectCrewMembers

-- >>>myCrew ^.. crewMembers
-- [Name {getName = "Grumpy Roger"},Name {getName = "Long-John Bronze"},Name {getName = "One-eyed Jack"},Name {getName = "Filthy Frank"}]

{-
#### Mapping over folds

`to`

- converts a function into a `Getter`.
- that's why, should never fail to get something from a structure.

- Book version:

  ```hs
  to :: (s -> a) -> Fold s a
  ```

- Real version:

  ```hs
  to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a
  class Profunctor (p :: Type -> Type -> Type) where
    dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  ```

  - [Profunctors](https://github.com/ocharles/blog/blob/master/guest-posts/2013-12-22-24-days-of-hackage-profunctors.md)

Example
-}

ex22 :: [Char]
ex22 = "Two-faced Tony" ^. to (take 2)

-- >>> ex22
-- "Tw"

{-
Composition
-}

-- >>> Name "Two-faced Tony" ^. to getName . to (fmap toUpper)
-- "TWO-FACED TONY"

-- >>> Name "Two-faced Tony" ^. to (fmap toUpper . getName)
-- "TWO-FACED TONY"

-- >>> myCrew ^.. crewMembers . to getName
-- ["Grumpy Roger","Long-John Bronze","One-eyed Jack","Filthy Frank"]

{-
#### Combining multiple folds on the same structure

-}

crewNames1 :: ShipCrew -> [Name]
crewNames1 sc = [captain, firstMate] ^.. folded . to (sc ^.) <> sc ^. conscripts

crewNames2 :: Fold ShipCrew Name
crewNames2 = folding (\s -> foldMap (s ^..) [captain, firstMate, conscripts . folded])

crewNames3 :: Fold ShipCrew Name
crewNames3 = folding (\s -> [captain, firstMate, conscripts . folded] ^.. folded . to (s ^..) . folded)

-- >>> myCrew ^.. crewNames2 . to getName
-- ["Grumpy Roger","Long-John Bronze","One-eyed Jack","Filthy Frank"]

{-
#### Exercises – Custom Folds

1. blanks
-}

{- LIMA_INDENT 4 -}

ex23 :: [Char]
ex23 = ["Yer" :: String, "a", "wizard", "Harry"] ^.. folded . folded

-- >>> ex23
-- "YerawizardHarry"

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)
-- [1,2,4,5]

-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)
-- [[1,2],[4,5]]

-- >>> ["bob", "otto", "hannah"] ^.. folded . to reverse
-- ["bob","otto","hannah"]

-- >>> ("abc", "def") ^.. folding (\(a, b) -> [a, b]). to reverse . folded
-- "cbafed"

{-
2. fold paths
-}

-- >>> [1..5] ^.. folded . folding (\x -> [x * 100])
-- [100,200,300,400,500]

-- >>> (1, 2) ^.. folding (\(a,b) -> [a, b])
-- [1,2]

-- >>> [(1, "one"), (2, "two")] ^.. folded . folding (\(_,x) -> [x])
-- ["one","two"]

ex24 :: [Int]
ex24 = (Just 1, Just 2, Just 3) ^.. folding (\(a, b, c) -> [a, b, c]) . folded

-- >>> ex24
-- [1,2,3]

ex25 :: [Int]
ex25 = [Left 1, Right 2, Left 3] ^.. folded . folded

-- >>> ex25
-- [2]

ex26 :: [Int]
ex26 = [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . folding (uncurry (<>))

-- >>> ex26
-- [1,2,3,4,5,6,7,8]

-- >>> [1, 2, 3, 4] ^.. folded . to (\x -> (if odd x then Left else Right) x)
-- [Left 1,Right 2,Left 3,Right 4]

-- >>> [(1, (2, 3)), (4, (5, 6))] ^.. folded . folding (\(a, (b,c)) -> [a,b,c])
-- [1,2,3,4,5,6]

ex27 :: [Integer]
ex27 = [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\(x, y) -> x ^.. folded <> y ^.. folded)

-- >>> ex27
-- [1,2]

ex28 :: [Either Integer String]
ex28 = [(1, "one"), (2, "two")] ^.. folded . folding (\(x, y) -> [Left x, Right y])

-- >>> ex28
-- [Left 1,Right "one",Left 2,Right "two"]

-- >>> S.fromList ["apricots", "apples"] ^.. folded . to reverse . folded
-- "selppastocirpa"

{-
3. outside of the box
-}

ex29 :: [Char]
ex29 = [(12, 45, 66), (91, 123, 87)] ^.. folded . folding (\(_, x, _) -> reverse (show x))

-- >>> ex29
-- "54321"

-- >>> [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded . folding (\(x,y) -> if odd x then [] else [y])
-- ["b","d"]

{- LIMA_DEDENT -}

{-
### 6.3 Fold Actions

Fold queries

- Which focuses match this **predicate**?
- What's the **largest** element in my structure
- What's the result of running this **side-effect** on every focus?
- What's the **sum** of these numeric focuses?
- Does this fold focus **any** elements?
- Does this **specific value** exist in my structure?

#### Writing queries with folds

There are folds for common functions like

- `minimumOf`
- `sumOf`

```hs
sumOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a
```

Instead of `Getting (Some type) s a`, can put many optics, e.g., `Fold s a`.

- `elemOf :: Eq a => Fold s a -> a -> s -> Bool` - does a fold contain an element?
- `anyOf :: Fold s a -> (a -> Bool) -> s -> Bool` - does any focus match a predicate?
- `allOf :: Fold s a -> (a -> Bool) -> s -> Bool` - do all focuses match a predicate?
- `findOf :: Fold s a -> (a -> Bool) -> s -> Maybe a` - find the first elem that matches a predicate
- `has :: Fold s a -> s -> Bool` - does my fold have any elements
- `hasn't :: Fold s a -> s -> Bool` - or not?
- `lengthOf :: Fold s a -> s -> Int` - how many focuses are there?
- `sumOf :: Num n => Fold s n -> s -> n` - sum of focuses
- `productOf :: Num n => Fold s n -> s -> n` - their product
- `firstOf :: Fold s a -> s -> Maybe a` - get the first focus
- `preview :: Fold s a -> s -> Maybe a` - like `firstOf`
- `(^?) :: s -> Fold s a -> Maybe a` - like `firstOf`
- `worded :: Fold String String` - like words
- `lastOf :: Fold s a -> s -> Maybe a` - get the last focus
- `minimumOf :: Ord a => Fold s a -> s -> Maybe a` - minimum
- `maximumOf :: Ord a => Fold s a -> s -> Maybe a` - maximum
- `maximumByOf :: Fold s a -> (a -> a -> Ordering) -> s -> Maybe a` - max element by a comparison func
- `folding :: Foldable f => (s -> f a) -> Fold s a` - convert structure to a `Foldable`
- `foldrOf :: Fold s a -> (a -> r -> r) -> r -> s -> r` - like foldr
- `foldlOf :: Fold s a -> (a -> r -> r) -> r -> s -> r` - like foldl
- `foldMapOf :: Monoid r => Fold s a -> (a -> r) -> s -> r` - like foldMap
- `foldByOf :: Fold s a -> (a -> a -> a) -> a -> s -> a` - lets use a custom `(<>) :: a -> a -> a`
- `foldMapByOf :: Fold s a -> (r -> r -> r) -> r -> (a -> r) -> s -> r` - same, but also lets map to a `Monoid`
-}

data Actor = Actor
  { _actorName :: String
  , _birthYear :: Int
  }
  deriving (Show, Eq)
makeLenses ''Actor

data TVShow = TVShow
  { _title :: String
  , _numEpisodes :: Int
  , _numSeasons :: Int
  , _criticScore :: Double
  , _actors :: [Actor]
  }
  deriving (Show, Eq)
makeLenses ''TVShow

howIMetYourMother :: TVShow
howIMetYourMother =
  TVShow
    { _title = "How I Met Your Mother"
    , _numEpisodes = 208
    , _numSeasons = 9
    , _criticScore = 83
    , _actors =
        [ Actor "Josh Radnor" 1974
        , Actor "Cobie Smulders" 1982
        , Actor "Neil Patrick Harris" 1973
        , Actor "Alyson Hannigan" 1974
        , Actor "Jason Segel" 1980
        ]
    }
buffy :: TVShow
buffy =
  TVShow
    { _title = "Buffy the Vampire Slayer"
    , _numEpisodes = 144
    , _numSeasons = 7
    , _criticScore = 81
    , _actors =
        [ Actor "Sarah Michelle Gellar" 1977
        , Actor "Alyson Hannigan" 1974
        , Actor "Nicholas Brendon" 1971
        , Actor "David Boreanaz" 1969
        , Actor "Anthony Head" 1954
        ]
    }

tvShows :: [TVShow]
tvShows =
  [ howIMetYourMother
  , buffy
  ]

-- >>> sumOf (folded . numEpisodes) tvShows
-- 352

comparingOf :: Ord a => Getting a s a -> s -> s -> Ordering
comparingOf l = comparing (view l)

ex30 :: Maybe Actor
ex30 = maximumByOf (folded . actors . folded) (comparingOf birthYear) tvShows

-- >>> ex30
-- Just (Actor {_actorName = "Cobie Smulders", _birthYear = 1982})

{-
#### Folding with effects

Effectful folding

- `traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()` - fold with effects

Similar to ordinary `Foldable` functions:

- `traverseOf_ :: Functor f => Fold s a -> (a -> f r) -> s -> f ()`
- `forOf_ :: Functor f => Fold s a -> s -> (a -> f r) -> f ()`

Uses just `Functor` (not `Applicative`) as `Lens` focuses a single element.
-}

calcAge :: Actor -> Int
calcAge actor = 2030 - actor ^. birthYear

showActor :: Actor -> String
showActor actor = actor ^. actorName <> ": " <> show (calcAge actor)

-- $> traverseOf_ (folded . actors . folded . to showActor) putStrLn tvShows

-- >>> import Control.Monad.State
-- >>> execState (traverseOf_ (folded . actors . folded) (modify . const (+1)) tvShows) 0
-- 10

{-
#### Combining fold results

`Fold`s are all about collecting pieces of things and `Monoid`s are all about combining
things together. We can find many focuses within a structure,
then combine the pieces together using a `Monoid`.

```hs
foldOf :: Getting a s a -> s -> a
foldMapOf :: Getting r s a -> (a -> r) -> s -> r
```

Implement an average fold
-}

ageSummary :: Actor -> (Sum Int, Sum Int)
ageSummary actor = (Sum 1, Sum (calcAge actor))

ex31 :: Double
ex31 = fromIntegral age / fromIntegral n
 where
  sums = foldMapOf (folded . actors . folded) ageSummary tvShows
  n = getSum (fst sums)
  age = getSum (snd sums)

-- >>> ex31
-- 57.2

{-
#### Using `view on folds`

Don't use `view` or `^.` on folds. It works only if focuses are `Monoid`s. Use `foldOf`
-}

-- >>> Just (42 :: Int) ^. folded
-- No instance for (Monoid Int) arising from a use of `folded'
-- In the second argument of `(^.)', namely `folded'
-- In the expression: Just (42 :: Int) ^. folded
-- In an equation for `it_a2Cc0O':
--     it_a2Cc0O = Just (42 :: Int) ^. folded

{-
#### Customizing monoidal folds

These functions allow customizing the (<>) operation on `Monoid`s

```hs
folding :: Foldable f => (s -> f a) -> Fold s a
foldByOf :: Fold s a -> (a -> a -> a) -> a -> s -> a
foldMapByOf :: Fold s a -> (r -> r -> r) -> r -> (a -> r) -> s -> r
foldrOf :: Fold s a -> (a -> r -> r) -> r -> s -> r
foldlOf :: Fold s a -> (r -> a -> r) -> r -> s -> r
```
-}

ex32 :: M.Map String Int
ex32 =
  foldMapByOf
    -- Focus each actor's name
    (folded . actors . folded . actorName)
    -- Combine duplicate keys with addition
    (M.unionWith (+))
    -- start with the empty Map
    mempty
    -- inject names into Maps with a count of 1
    (`M.singleton` 1)
    tvShows

-- >>> ex32
-- fromList [("Alyson Hannigan",2),("Anthony Head",1),("Cobie Smulders",1),("David Boreanaz",1),("Jason Segel",1),("Josh Radnor",1),("Neil Patrick Harris",1),("Nicholas Brendon",1),("Sarah Michelle Gellar",1)]

{-
#### Exercises – Fold Actions

1. pick action
-}

{- LIMA_INDENT 4 -}

-- >>> has folded []
-- False

-- >>> foldOf both ("Yo", "Adrian!")
-- "YoAdrian!"

-- >>> elemOf each "phone" ("E.T.", "phone", "home")
-- True

-- >>> minimumOf folded [5, 7, 2, 3, 13, 17, 11]
-- Just 2

-- >>> maximumOf folded [5, 7, 2, 3, 13, 17, 11]
-- Just 17

-- >>> anyOf folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
-- True

-- >>> findOf folded even [11, 22, 3, 5, 6]
-- Just 22

{-
2. devise folds
-}

ex33 :: Maybe String
ex33 = findOf folded (\x -> x == reverse x) ["umbrella", "olives", "racecar", "hammer"]

-- >>>ex33
-- Just "racecar"

-- >>>allOf each even (2,4,6)
-- True

ex34 :: Maybe (Int, String)
ex34 = maximumByOf folded (\x y -> compare (x ^. _1) (y ^. _1)) [(2 :: Int, "I'll" :: String), (3, "Be"), (1, "Back")]

-- >>> ex34
-- Just (3,"Be")

-- >>> sumOf each (1,2)
-- 3

{-
3. bonus
-}

isVowel :: Char -> Bool
isVowel x = x `elem` ("aouiey" :: String)

ex35 :: Maybe String
ex35 =
  maximumByOf
    worded
    (\x y -> let s = (length . filter isVowel) in compare (s x) (s y))
    ("Do or do not, there is no try." :: String)

-- >>> ex35
-- Just "there"

{- LIMA_DEDENT -}

{-
### 6.4 Higher Order Folds

There're optics combinators that **alter other optics**. They accept an optic and return a new one.

(with simplified types)

- `taking :: Int -> Fold s a -> Fold s a` - like `take`
- `dropping :: Int -> Fold s a -> Fold s a` - like `drop`
- `takingWhile :: (a -> Bool) -> Fold s a -> Fold s a` - like `takeWhile`
- `droppingWhile :: (a -> Bool) -> Fold s a -> Fold s a` - like `dropWhile`
- `backwards :: Fold s a -> Fold s a` - reverse the order of focuses of a fold

#### Taking, Dropping

(real types are complex)

```hs
taking :: Int -> Fold s a -> Fold s a
dropping :: Int -> Fold s a -> Fold s a
```
-}

-- >>>[3,5,4,6,7] ^.. taking 3 folded
-- [3,5,4]

-- >>>[3,5,4,6,7] ^.. dropping 3 folded
-- [6,7]

{-
Since new folds branch on focuses, the next optics are applied on each branch separately.
-}

-- >>> [[1, 2, 3], [10, 20, 30], [100, 200, 300]] ^.. folded . taking 2 folded
-- [1,2,10,20,100,200]

-- >>> ("Albus" :: String, "Dumbledore") ^.. both . taking 3 folded
-- "AlbDum"

{-
We can move the combinator to operate on the necessary set of focuses, e.g., the final one.
-}

-- No brackets; we're taking '3' from the results of 'both', then folding them
-- >>> ("Albus" :: String, "Dumbledore") ^.. taking 3 both . folded
-- "AlbusDumbledore"

-- >>> ("Albus" :: String, "Dumbledore") ^.. taking 3 (both . folded)
-- "Alb"

-- >>> ("Albus" :: String, "Dumbledore") ^.. dropping 2 (both . folded)
-- "busDumbledore"

{-
#### Backwards

Reverses the order of a fold.

Real:

  ```hs
  backwards :: (Profunctor p, Profunctor q) => Optical p q (Backwards f) s t a b -> Optical p q f s t a b
  ```

Book:

  ```hs
  backwards :: Fold s a -> Fold s a
  ```
-}

-- >>> [1, 2, 3] ^.. backwards folded
-- [3,2,1]

{-
#### takingWhile, droppingWhile
-}

-- >>> [1..100] ^.. takingWhile (<10) folded
-- [1,2,3,4,5,6,7,8,9]

-- >>> [1..100] ^.. droppingWhile (<90) folded
-- [90,91,92,93,94,95,96,97,98,99,100]

{-
#### Exercises – Higher Order Folds

1. blanks
-}

{- LIMA_INDENT 4 -}

-- >>> ("Here's looking at you, kid" :: String) ^.. dropping 7 folded
-- "looking at you, kid"

-- >>> ["My Precious" :: String, "Hakuna Matata", "No problemo"] ^.. folded . taking 1 .
-- "MHN"

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. taking 1 (folded . worded)
-- ["My"]

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 worded . folded
-- "MyHakunaNo"

-- >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 (folding words) . folded
-- "MyHakunaNo"

ex36 :: Integer
ex36 = sumOf (taking 2 each) (10, 50, 100)

-- >>> ex36
-- 60

-- >>> ("stressed", "guns", "evil") ^.. backwards each
-- ["evil","guns","stressed"]

-- >>> ("stressed", "guns", "evil") ^.. backwards each . to reverse
-- ["live","snug","desserts"]

-- >>> import Data.Char (isAlpha)
-- >>> "blink182 k9 blazeit420" ^.. folding (filter (\x -> not (isAlpha x || x == ' ')))
-- "1829420"

{-
2. use higher-order folds
-}

temperatureSample :: [Int]
temperatureSample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]

-- >>> length $ temperatureSample ^.. takingWhile (<= 0) folded
-- 2

-- >>> maximumOf (taking 4 folded) temperatureSample
-- Just 4

-- >>> temperatureSample ^? dropping 1 (droppingWhile (/= 4) folded)
-- Just 3

-- >>> length $ temperatureSample ^.. takingWhile (< 0) (backwards folded)
-- 2

-- >>> temperatureSample ^.. takingWhile (> 0) (droppingWhile (<= 0) folded)
-- [4,3,8,6]

trimmingWhile :: (a -> Bool) -> Fold s a -> Fold s a
trimmingWhile c f = backwards (droppingWhile c (backwards (droppingWhile c f)))

-- >>> temperatureSample ^.. trimmingWhile (< 0) folded
-- [4,3,8,6,-2,3]

{- LIMA_DEDENT -}

{-
### 6.5 Filtering folds

- Filter focuses (like WHERE in SQL)
- Can run a separate fold to calculate the filter condition
- Can go deeper after filtering

Book

- `filtered :: (s -> Bool) -> Fold s s` - filter a fold
- `filteredBy :: Fold s a -> Fold s s` or `filteredBy :: Fold s a -> IndexedTraversal' a s s` - filter by a condition represented as a fold

Real

- `filtered :: (Choice p, Applicative f) => (a -> Bool) -> Optic' p f a a`
- `filteredBy :: (Indexable i p, Applicative f) => Getting (First i) a i -> p a (f a) -> a -> f a`

-}

-- >>> [1, 2, 3, 4] ^.. folded . filtered even
-- [2,4]

-- >>> ["apple", "passionfruit", "orange", "pomegranate"] ^.. folded . filtered ((>6) . length)
-- ["passionfruit","pomegranate"]

-- A data structure to represent a single card
data Card = Card
  { _cardName :: String
  , _aura :: Aura
  , _holo :: Bool
  , _moves :: [Move]
  }
  deriving (Show, Eq)

-- Each card has an aura-type
data Aura
  = Wet
  | Hot
  | Spark
  | Leafy
  deriving (Show, Eq)

-- Cards have attack moves
data Move = Move
  { _moveName :: String
  , _movePower :: Int
  }
  deriving (Show, Eq)

makeLenses ''Card
makeLenses ''Move

deck :: [Card]
deck =
  [ Card "Skwortul" Wet False [Move "Squirt" 20]
  , Card "Scorchander" Hot False [Move "Scorch" 20]
  , Card "Seedasaur" Leafy False [Move "Allergize" 20]
  , Card "Kapichu" Spark False [Move "Poke" 10, Move "Zap" 30]
  , Card "Elecdude" Spark False [Move "Asplode" 50]
  , Card "Garydose" Wet True [Move "Gary's move" 40]
  , Card "Moisteon" Wet False [Move "Soggy" 3]
  , Card "Grasseon" Leafy False [Move "Leaf Cut" 30]
  , Card "Spicyeon" Hot False [Move "Capsaicisize" 40]
  , Card "Sparkeon" Spark True [Move "Shock" 40, Move "Battery" 50]
  ]

{-
- How many moves have an attack power above 30?
-}

{- LIMA_INDENT 4 -}

ex38 :: Int
ex38 =
  lengthOf
    ( folded
        . moves
        . folded
        . movePower
        . filtered (> 30)
    )
    deck

-- >>> ex38
-- 5

{-
- List all cards which have ANY move with an attack power greater than 40
-}

ex39 :: [String]
ex39 =
  deck
    ^.. folded
      . filtered (anyOf (moves . folded . movePower) (> 40))
      . cardName

-- >>> ex39
-- ["Elecdude","Sparkeon"]

{-
- List all Spark Moves with a power greater than 30
-}

-- ex40 :: [Move]
ex40 :: [String]
ex40 =
  deck
    ^.. folded
      . filtered (\x -> x ^. aura == Spark)
      . moves
      . folded
      . filtered (\x -> x ^. movePower > 30)
      . moveName

-- >>>ex40
-- ["Asplode","Shock","Battery"]

{-
Other helpers

- `filteredBy :: Fold s a -> Fold s s` - filter by a condition represented as a fold
- `only :: Eq a => a -> Prism' a ()` - return () iff input is equal to a reference value
- `nearly :: a -> (a -> Bool) -> Prism' a ()` - check condition. As it returns a prism, we have to supply the first argument for re-construction

-}

-- >>> has (only "needle") "needle"
-- True

{-
- List all Spark Moves with a power greater than 30
-}

ex41 :: [String]
ex41 =
  deck
    ^.. folded
      . filteredBy (aura . only Spark)
      . moves
      . folded
      . filteredBy (movePower . filtered (> 30))
      . moveName

-- >>> ex41
-- ["Asplode","Shock","Battery"]

ex42 :: Maybe String
ex42 =
  maximumByOf
    -- filter for holo cards
    (folded . filteredBy holo)
    -- compare them on number of moves
    (comparing (lengthOf moves))
    deck
    <&> (^. cardName)

-- >>> ex42
-- Just "Sparkeon"

{- LIMA_DEDENT -}

{-
#### Exercises – Filtering
-}

{-
- List all the cards whose name starts with 'S'
-}

{- LIMA_INDENT 4 -}

ex43 :: [String]
ex43 = deck ^.. folded . filteredBy (cardName . taking 1 folded . only 'S') . cardName

-- >>> ex43
-- ["Skwortul","Scorchander","Seedasaur","Spicyeon","Sparkeon"]

{-
- What's the lowest attack power of all moves?
-}
ex44 :: Maybe Int
ex44 = minimumOf (folded . moves . folded . movePower) deck

-- >>>ex44
-- Just 3

{-
- What's the name of the first card which has more than one move?
-}

ex45 :: Maybe String
ex45 = findOf (folded . filtered (\x -> length (x ^. moves) > 1)) (const True) deck <&> (^. cardName)

-- >>>ex45
-- Just "Kapichu"

{-
- Are there any Hot cards with a move with more than 30 attack power?
-}

ex46 :: Bool
ex46 =
  not . null $
    deck
      ^.. folded
        . filteredBy (aura . only Hot)
        . filteredBy (moves . folded . filteredBy (movePower . nearly 0 (> 30)))

-- >>>ex46
-- [Card {_cardName = "Spicyeon", _aura = Hot, _holo = False, _moves = [Move {_moveName = "Capsaicisize", _movePower = 40}]}]

{-
- List the names of all holographic cards with a Wet aura.
-}

ex47 :: [String]
ex47 = deck ^.. folded . filtered (\x -> x ^. holo && x ^. aura == Wet) . cardName

-- >>>ex47
-- ["Garydose"]

{-
- What's the sum of all attack power for all moves belonging to non-Leafy cards?
-}

ex48 :: Int
ex48 = sumOf (folded . filtered (\x -> x ^. aura /= Leafy) . moves . folded . movePower) deck

-- >>>ex48
-- 303

{- LIMA_DEDENT -}

{-
### 7. Traversals

#### 7.1. Introduction to Traversals

Can get or set many focuses **in-place**.

- **rows** - optics that we **have**
- **columns** - how want to **use** that optics

![alt](README/tableTraversals.png)

#### From fold to traversal

```hs
both :: Bitraversable r => Traversal (r a a) (r b b) a b
```

In case of tuples, `both` focuses both sides of a tuple.

`Traversal s t a b`:

- `s`: structure before action
- `t`: structure after action
- `a`: focus before action
- `b`: focus after action

Let's modify both elements of a tuple
-}

ex49 :: (String, String)
ex49 = ("Bubbles", "Buttercup") & both %~ (++ "!")

-- >>> ex49
-- ("Bubbles!","Buttercup!")

{-
Focuses may change type as long as the type of a structure remains valid. In case of each, we have to change types of all elements of a tuple.
-}

-- >>> ("Bubbles", "Buttercup") & each %~ length
-- (7,9)

-- >>> [1, 2, 3, 4, 5] & dropping 3 traversed %~ show
-- No instance for (Num String) arising from the literal `1'
-- In the expression: 1
-- In the first argument of `(&)', namely `[1, 2, 3, 4, 5]'
-- In the expression: [1, 2, 3, 4, 5] & dropping 3 traversed %~ show

{-
Some structures disallow changing the type.
-}

-- >>> ("Houston we have a problem" :: T.Text) & each .~ (22 :: Int)
-- Couldn't match type `Int' with `Char' arising from a use of `each'
-- In the first argument of `(.~)', namely `each'
-- In the second argument of `(&)', namely `each .~ (22 :: Int)'
-- In the expression:
--   ("Houston we have a problem" :: Text) & each .~ (22 :: Int)

{-
Can use some functions that we used for `Fold`s, e.g., `filtered`.

-}

-- Reverse only the long strings
ex50 :: (String, String)
ex50 =
  ("short", "really long")
    & both . filtered ((> 5) . length)
      %~ reverse

-- >>>ex50
-- ("short","gnol yllaer")

{-
### 7.2 Traversal Combinators

#### Traversing each element of a container

Some optics are incompatible in types, e.g., `folded` and `%~`. That is, you can't modify focuses in a fold
-}

-- >>> [1, 2, 3] & folded %~ (*10)
-- Could not deduce (Contravariant Identity)
--   arising from a use of `folded'
-- from the context: Num b_aNbRI[sk:1]
--   bound by the inferred type of
--              it_aNbPv :: Num b_aNbRI[sk:1] => [b_aNbRI[sk:1]]
--   at /home/eyjafjallajokull/Desktop/projects/optics-by-example/README.hs:2207:2-28
-- In the first argument of `(%~)', namely `folded'
-- In the second argument of `(&)', namely `folded %~ (* 10)'
-- In the expression: [1, 2, 3] & folded %~ (* 10)

{-
That's why there is a specific function for traversing.

Book:

```hs
traversed :: Traversable f => Traversal (f a) (f b) a b
```

Real:
```hs
traversed :: Traversable f => IndexedTraversal Int (f a) (f b) a b
class (Functor t, Foldable t) => Traversable t
```

If you compose a `Traversal` and a `Fold`, you get a `Fold`.
-}

-- >>>[[3 :: Int, 4]] & traversed . folded %~ (*10)
-- No instance for (Contravariant Identity)
--   arising from a use of `folded'
-- In the second argument of `(.)', namely `folded'
-- In the first argument of `(%~)', namely `traversed . folded'
-- In the second argument of `(&)', namely
--   `traversed . folded %~ (* 10)'

-- >>>[[3 :: Int, 4]] ^.. traversed . folded
-- [3,4]

{-
Compared to **folded**, **traversed** operates on **less** containers with **more** operations.
-}

powerLevels :: M.Map String Integer
powerLevels =
  M.fromList
    [ ("Gohan", 710)
    , ("Goku", 9001)
    , ("Krillin", 5000)
    , ("Piccolo", 408)
    ]

-- operate on the values of a map
ex51 :: M.Map String String
ex51 =
  powerLevels
    & traversed %~ \n ->
      if n > 9000
        then "Over 9000"
        else show n

-- >>>ex51
-- fromList [("Gohan","710"),("Goku","Over 9000"),("Krillin","5000"),("Piccolo","408")]

{-
#### More Combinators

Book:

- `worded :: Traversal' String String` - focus on words
- `lined :: Traversal' String String` - focus on lines

Real:

- `worded :: Applicative f => IndexedLensLike' Int f String String`
- `lined :: Applicative f => IndexedLensLike' Int f String String`

Unlawful, because they wrongly reconstruct the results. E.g., like `unwords . words`, they substitute a single space for multiple spaces.
-}

-- >>> "blue \n suede \n \n shoes" & worded %~ \(x:xs) -> toUpper x : xs
-- "Blue Suede Shoes"

{-
#### Traversing multiple paths at once

Focus on all `a`s from both structures in a tuple.

```hs
beside :: Traversal s t a b -> Traversal s' t' a b -> Traversal (s,s') (t,t') a b
beside :: Lens s t a b      -> Lens s' t' a b      -> Traversal (s,s') (t,t') a b
beside :: Fold s a          -> Fold s' a           -> Fold (s,s') a
```
-}

-- >>> let dinos = ("T-Rex", (42, "Stegosaurus"))
-- >>>  dinos ^.. beside id _2
-- ["T-Rex","Stegosaurus"]

ex52 :: (String, [String])
ex52 =
  ("Cowabunga", ["let's", "order", "pizza"])
    -- Each half of the tuple has a different path to focus the characters
    & beside traversed (traversed . traversed)
      %~ toUpper

-- >>>ex52
-- ("COWABUNGA",["LET'S","ORDER","PIZZA"])

{-
There are other `Bitraversable`s like `Either`.
-}

-- >>> Left (1, 2) & beside both traversed %~ negate
-- Left (-1,-2)

{-
#### Focusing a specific traversal element

Focuses a single element with a given index. Can't change the type of that focus because it can't change the type of other focuses.

```hs
element :: Traversable f => Int -> Traversal' (f a) a
```
-}

-- >>> [0, 1, 2, 3, 4] & element 2 *~ 100
-- [0,1,200,3,4]

{-
Focus an element of a traversal or a fold

```hs
elementOf :: Traversal' s a -> Int -> Traversal' s a
elementOf :: Fold s a       -> Int -> Fold s a
```
-}

-- >>> [[0, 1, 2], [3, 4], [5, 6, 7, 8]] & elementOf (traversed . traversed) 6 *~ 100
-- [[0,1,2],[3,4],[5,600,7,8]]

{-
### 7.3 Traversal Composition
-}

-- Add "Rich " to the names of people with more than $1000
ex53 :: ((String, Integer), (String, Integer), (String, Integer))
ex53 =
  (("Ritchie", 100000), ("Archie", 32), ("Reggie", 4350))
    & each
      . filtered ((> 1000) . snd)
      . _1
      %~ ("Rich " ++)

-- >>>ex53
-- (("Rich Ritchie",100000),("Archie",32),("Rich Reggie",4350))

{-
#### Exercises – Simple Traversals

1. What type of optic do you get when you compose a traversal with a fold?
    - fold
-}

{- LIMA_INDENT 6 -}

-- >>> [[3 :: Int, 4]] ^.. traversed . folded
-- [3,4]

-- >>> [[3 :: Int, 4]] & traversed . folded .~ 2
-- No instance for (Contravariant Identity)
--   arising from a use of `folded'
-- In the second argument of `(.)', namely `folded'
-- In the first argument of `(.~)', namely `traversed . folded'
-- In the second argument of `(&)', namely `traversed . folded .~ 2'

{-
1. Which of the optics we’ve learned can act as a traversal?
    - lens and traversal

1.  Which of the optics we’ve learned can act as a fold?
    - lens, traversal, fold
-}

{- LIMA_DEDENT 6 -}

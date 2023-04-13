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
{-# LANGUAGE DuplicateRecordFields #-}

{- LIMA_DISABLE -}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- LIMA_ENABLE -}

{- FOURMOLU_ENABLE -}

module Main (main) where

import Control.Applicative (Applicative (..))
import Control.Lens
import Control.Lens (_1)
import Control.Lens.Unsound (lensProduct)
import Control.Monad.State
import Data.ByteString qualified as BS
import Data.Char (isUpper, toLower, toUpper)
import Data.Either.Validation
import Data.Foldable (Foldable (..))
import Data.List
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, toList)
import Data.Map (fromList)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Ord (comparing)
import Data.Set qualified as S (Set (..), fromList)
import Data.Text qualified as T
import Data.Text.Lens (unpacked)
import Data.Tree (Tree (..))
import GHC.Word qualified
import Numeric.Lens (adding, multiplying, negated)
import Text.Read (readMaybe)

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

2. solution:
-}

{- LIMA_INDENT 4 -}

ex1 :: Lens' (Char, Int) Char
ex1 = undefined

{-
3. Lens actions:

    - get
    - set
    - modify

4. focus on `c`
-}

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

{-
1. apply lens
-}

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

2. Rewrite
-}

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

{-
2. Can't get from `Nothing`, so, can't have `inMaybe :: Lens' (Maybe a) a` not fail sometimes
-}

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

{-
3. fail `get-set`, pass other
-}

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

{-
4. like `msg1`
-}

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

{-
5. break all rules
-}

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

{-
6. builder
-}

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
  , _userEmail :: String
  }
  deriving (Show)

makeLenses ''User

username :: Lens' User String
username = lens getter setter
 where
  getter = view userEmail
  setter user_ s = set userEmail s user_

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

1. `Vorpal`
-}

{- LIMA_INDENT 4 -}

data Vorpal a

vorpal :: Lens (Vorpal a) (Vorpal b) a b
vorpal = undefined

{-
2. Polymorphic unlawful
-}

data Preferences a = Preferences {_best :: a, _worst :: a} deriving (Show)

best :: Lens (Preferences a) (Preferences b) a b
best = lens getter setter
 where
  getter (Preferences a _) = a
  setter (Preferences _ _) c = Preferences{_best = c, _worst = c}

{-
3. Result
-}

data Result e = Result {_lineNumber :: Int, _result :: Either e String}

result :: Lens (Result a) (Result b) a b
result = undefined

{-
4. Multiple
-}

data Multi a b

multi :: Lens (Multi a b) (Multi c d) (a, b) (c, d)
multi = undefined

{-
5. Predicate
-}

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

{-
2. Domino
-}

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

{-
3. Rewrite
-}

data Armadillo
data Hedgehog
data Platypus
data BabySloth

g :: Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
g = undefined

h :: Lens Platypus BabySloth Armadillo Hedgehog
h = undefined

{-
4. Compose
-}

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

{- LIMA_DEDENT -}

{-
## 5. Operators

<b>Fixity</b> - operator precedence
-}

-- >>>:t _1 . _2 .~ 3
-- _1 . _2 .~ 3 :: (Field1 s t a1 b1, Field2 a1 b1 a2 b2, Num b2) => s -> t

{-
is equivalent to
-}

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

{-
Or even
-}

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

### 5.9 Exercises – Operators

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
ex19 = toListOf (folded . folded) (M.fromList [("Jack" :: String, "Captain" :: String), ("Will", "First Mate")])

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

- Book:

  ```hs
  to :: (s -> a) -> Fold s a
  ```

- Real:
-}

-- >>>:t to
-- to :: (Profunctor p, Contravariant f) => (s -> a) -> Optic' p f s a

{-
  ```hs
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

take N focuses

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

Book:

  ```hs
  backwards :: Fold s a -> Fold s a
  ```

Real:
-}

-- >>>:t backwards
-- backwards
--   :: (Profunctor p, Profunctor q) =>
--      Optical p q (Backwards f) s t a b -> Optical p q f s t a b

{-
Examples:
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

Book:

- `filtered :: (s -> Bool) -> Fold s s` - filter a fold
- `filteredBy :: Fold s a -> Fold s s` or `filteredBy :: Fold s a -> IndexedTraversal' a s s` - filter by a condition represented as a fold

Real:
-}

-- >>>:t filtered
-- filtered :: (Choice p, Applicative f) => (a -> Bool) -> Optic' p f a a

-- >>>:t filteredBy
-- filteredBy
--   :: (Indexable i p, Applicative f) =>
--      Getting (First i) a i -> p a (f a) -> a -> f a

{-
Examples:
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

Have multiple focuses. Can transform them.

#### 7.1. Introduction to Traversals

Can get or set many focuses **in-place**.

- **rows** - optics that we **have**
- **columns** - how want to **use** that optics

![alt](README/tableTraversals.png)

#### From Fold to Traversal

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
-}

-- >>> :t traversed
-- traversed :: Traversable f => IndexedTraversal Int (f a) (f b) a b

{-
```hs
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
-}

-- >>> :t worded
-- worded :: Applicative f => IndexedLensLike' Int f String String

-- >>> :t lined
-- lined :: Applicative f => IndexedLensLike' Int f String String

{-
They're unlawful, because they wrongly reconstruct the results. E.g., like `unwords . words`, they substitute a single space for multiple spaces.
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

Focuses a single element with a given index.
Can't change the type of that focus because it can't change the type of other focuses.

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

1. Which of the optics we’ve learned can act as a fold?
    - lens, traversal, fold
-}

{- LIMA_DEDENT -}

-- >>>("Jurassic", "Park") & both .~ "N/A"
-- ("N/A","N/A")

-- >>> ("Jurassic" :: String, "Park") & both . traversed .~ 'x'
-- ("xxxxxxxx","xxxx")

-- >>>("Malcolm", ["Kaylee", "Inara", "Jayne"]) & beside id traversed %~ take 3
-- ("Mal",["Kay","Ina","Jay"])

-- >>>("Malcolm", ["Kaylee", "Inara", "Jayne"]) & _2 . elementOf traversed 1 .~ "River"
-- ("Malcolm",["Kaylee","River","Jayne"])

-- >>> ["Die Another Day", "Live and Let Die", "You Only Live Twice"] & traversed . elementOf worded 1 . traversed .~ 'x'
-- ["Die xxxxxxx Day","Live xxx Let Die","You xxxx Live Twice"]

-- >>>((1, 2), (3, 4)) & both . both +~ 1
-- ((2,3),(4,5))

-- >>>(1, (2, [3, 4])) & beside id (beside id traversed) +~ 1
-- (2,(3,[4,5]))

ex54 = ((True, "Strawberries" :: String), (False, "Blueberries"), (True, "Blackberries")) & each . filtered fst . _2 . taking 5 traversed %~ toUpper

-- >>> ex54
-- ((True,"STRAWberries"),(False,"Blueberries"),(True,"BLACKberries"))

ex55 = ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries" :: String)) & each %~ snd

-- >>> ex55
-- ("Strawberries","Blueberries","Blackberries")

{-
### 7.4 Traversal Actions

```hs
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
```
-}

-- >>>sequenceA $ Just (Left "Whoops")
-- Left "Whoops"

-- >>>sequenceA $ Just (Right "Whoops")
-- Right (Just "Whoops")

-- >>> :t readMaybe
-- readMaybe :: Read a => String -> Maybe a

-- >>>traverse readMaybe ["1", "2", "3"] :: Maybe [Int]
-- Just [1,2,3]

-- >>>traverse readMaybe ["1", "snark", "3"] :: Maybe [Int]
-- Nothing

{-
#### Traverse on Traversals

Can run `traverse` on arbitrary focuses!
-}

-- >>>:t traverseOf
-- traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t

-- >>> :t traverse
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

-- >>> :t traverseOf traversed
-- traverseOf traversed
--   :: (Traversable f1, Applicative f2) =>
--      (a -> f2 b) -> f1 a -> f2 (f1 b)

-- >>>traverseOf both readMaybe ("1", "2") :: Maybe (Int, Int)
-- Just (1,2)

-- >>> traverseOf both (\c -> [toLower c, toUpper c]) ('a', 'b')
-- [('a','b'),('a','B'),('A','b'),('A','B')]

-- >>> traverseOf (both . traversed) (\c -> [toLower c, toUpper c]) ("ab", "c")
-- [("ab","c"),("ab","C"),("aB","c"),("aB","C"),("Ab","c"),("Ab","C"),("AB","c"),("AB","C")]

validateEmail :: String -> Validation [String] String
validateEmail email
  | elem '@' email = Success email
  | otherwise =
      Failure ["missing '@': " <> email]

-- >>> traverseOf (both . traversed) validateEmail (["mike@tmnt.io", "raph@tmnt.io"], ["don@tmnt.io", "leo@tmnt.io"])
-- Success (["mike@tmnt.io","raph@tmnt.io"],["don@tmnt.io","leo@tmnt.io"])

-- >>> traverseOf (both . traversed) validateEmail (["mike@tmnt.io", "raph.io"], ["don@tmnt.io", "leo.io"])
-- Failure ["missing '@': raph.io","missing '@': leo.io"]

{-
Other functions:
-}

-- >>>:t forOf
-- forOf :: LensLike f s t a b -> s -> (a -> f b) -> f t

-- >>>:t sequenceAOf
-- sequenceAOf :: LensLike f s t (f b) b -> s -> f t

-- >>> sequenceAOf _1 (Just "Garfield", "Lasagna")
-- Just ("Garfield","Lasagna")

-- >>> sequenceAOf (both . traversed) ([Just "apples"], [Just "oranges"])
-- Just (["apples"],["oranges"])

{-
#### Infix `traverseOf`
-}

-- >>> (("1", "2") & both %%~ readMaybe) :: Maybe (Int, Int)
-- Just (1,2)

{-
#### Use Traversals directly

Actual definitions:

```hs
traverseOf = id
(%%~) = id
```

So, we can (but should not!) use `Traversal`s without `traverseOf`:
-}

-- >>>both readMaybe ("1", "2") :: Maybe (Int, Int)
-- Just (1,2)

{-
#### Exercises - Traversal Actions
-}

-- >>> sequenceAOf _1 (Nothing, "Rosebud")
-- Nothing

-- >>> sequenceAOf (traversed . _1) [("ab" :: String,1),("cd",2)]
-- [[('a',1),('c',2)],[('a',1),('d',2)],[('b',1),('c',2)],[('b',1),('d',2)]]

ex56 :: (([Integer], (Integer, Integer)), Integer)
ex56 = runState result 0
 where
  result = traverseOf (beside traversed both) (\n -> modify (+ n) >> get) ([1, 1, 1], (1, 1))

-- >>>ex56
-- (([1,2,3],(4,5)),5)

ex57 :: [([Char], Bool)]
ex57 =
  ("ab" :: String, True)
    & (_1 . traversed)
      %%~ (\c -> [toLower c, toUpper c])

ex58 :: [[(Char, Bool)]]
ex58 =
  [('a', True), ('b', False)]
    & (traversed . _1)
      %%~ (\c -> [toLower c, toUpper c])

data UserWithAge = UserWithAge
  { _userName :: String
  , _userAge :: Int
  }
  deriving (Show)
makeLenses ''UserWithAge
data Account = Account
  { _accountId :: String
  , _userWithAge :: UserWithAge
  }
  deriving (Show)
makeLenses ''Account

validateAge :: Account -> Validation String Account
validateAge acc
  | age' <= 0 = Failure "Age is below 0"
  | age' >= 150 = Failure "Age is above 150"
  | otherwise = Success acc
 where
  age' = acc ^. userWithAge . userAge

{-
### 7.5 Custom traversals

van Laarhoven optics are

```hs
type LensLike f s t a b = (a -> f b) -> (s -> f t)
```

plus constraints

```hs
type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> (s -> f t)
type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> (s -> f s)
```

And LensLike is very similar to `traverse` signature:

```hs
traverse :: (Traversable g, Applicative f) => (a -> f b) -> (g a -> f (g b))
myTraversal :: myTraversal :: (Applicative f) => (a -> f b) -> (s -> f t)
```

<b>Most optics are really just traverse wearing different pants.</b>

#### Our first custom traversal

`traversed` for lists
-}

-- values :: Traversal [a] [b] a b
values :: Applicative f => (a -> f b) -> [a] -> f [b]
values _ [] = pure []
values handler (a : as) = liftA2 (:) (handler a) (values handler as)

-- >>> ["one", "two", "three"] & values %~ length
-- [3,3,5]

{-
#### Traversals with custom logic

Some bank software
-}

data Transaction
  = Withdrawal {_amount :: Int}
  | Deposit {_amount :: Int}
  deriving (Show)
makeLenses ''Transaction

newtype BankAccount = BankAccount
  { _transactions :: [Transaction]
  }
  deriving (Show)
makeLenses ''BankAccount

aliceAccount :: BankAccount
aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]

-- >>>aliceAccount ^.. transactions . traversed . amount
-- [100,20,10]

{-
#### Case study: Transaction Traversal

Need a traversal which focuses on only the dollar amounts of **deposits** within a given account.
-}

-- deposits :: Traversal' [Transaction] Int
-- deposits :: Traversal [Transaction] [Transaction] Int Int
deposits :: Applicative f => (Int -> f Int) -> [Transaction] -> f [Transaction]
deposits _ [] = pure []
deposits handler (Withdrawal amt : rest) = fmap (Withdrawal amt :) (deposits handler rest)
deposits handler (Deposit amt : rest) = liftA2 (:) (Deposit <$> handler amt) (deposits handler rest)

-- >>>[Deposit 10, Withdrawal 20, Deposit 30] & deposits *~ 10
-- [Deposit {_amount = 100},Withdrawal {_amount = 20},Deposit {_amount = 300}]

deposits' :: Traversal' [Transaction] Int
deposits' = traversed . filtered (\case Deposit _ -> True; _ -> False) . amount

{-
#### Exercises - Custom traversals

1. custom traversal
-}

{- LIMA_INDENT 4 -}

-- amountT :: Traversal' Transaction Int
amountT :: Applicative f => (Int -> f Int) -> Transaction -> f Transaction
amountT f = \case Deposit am -> Deposit <$> f am; Withdrawal am -> Withdrawal <$> f am

{-
2. custom `both`
-}

both' :: Traversal (a, a) (b, b) a b
both' f (x, y) = liftA2 (,) (f x) (f y)

{-
3. delta - Similar to change of coordinates via matrix pre- and post-multiplication
-}

transactionDelta :: Traversal' Transaction Int
transactionDelta f = \case Deposit amt -> Deposit <$> f amt; Withdrawal amt -> Withdrawal . negate <$> f (negate amt)

-- >>> Deposit 10 ^? transactionDelta
-- Just 10

-- Withdrawal's delta is negative
-- >>> Withdrawal 10 ^? transactionDelta
-- Just (-10)
-- >>> Deposit 10 & transactionDelta .~ 15
-- Deposit {_amount = 15}
-- >>> Withdrawal 10 & transactionDelta .~ (-15)
-- Withdrawal {_amount = 15}
-- >>> Deposit 10 & transactionDelta +~ 5
-- Deposit {_amount = 15}
-- >>> Withdrawal 10 & transactionDelta +~ 5
-- Withdrawal {_amount = 5}

{- LIMA_DEDENT -}

left' :: Traversal (Either a b) (Either a' b) a a'
left' f = \case Left e -> Left <$> f e; Right x -> pure $ Right x

beside' :: Traversal s t a b -> Traversal s' t' a b -> Traversal (s, s') (t, t') a b
beside' l r f (l1, r1) = liftA2 (,) (l f l1) (r f r1)

{-
### 7.6 Traversal Laws

#### Law One: Respect Purity

Running the pure handler (which has no effects) using our traversal should be exactly
the same as running `pure` on the original structure without using the traversal at all.

  ```hs
  traverseOf myTraversal pure x == pure x
  ```
-}

badTupleSnd :: Traversal (Int, a) (Int, b) a b
badTupleSnd handler (n, a) = (n + 1,) <$> handler a

-- >>> traverseOf badTupleSnd pure (10, "Yo")
-- (11,"Yo")

{-
#### Law Two: Consistent Focuses

Running a traversal twice in a row with **different** handlers should be equivalent
to running it **once** with the composition of those handlers.

  ```hs
  x & myTraversal %~ f
    & myTraversal %~ g
  ==
  x & myTraversal %~ (g . f)
  ```

The traversal should never change which elements it focuses due to
alterations on those elements.

`filtered` breaks this law!
-}

-- >>> 2 & filtered even %~ (+1) & filtered even %~ (*10)
-- 3

-- >>> 2 & filtered even %~ (*10) . (+1)
-- 30

{-
#### Exercises – Traversal Laws

1. `worded` violates the Law Two
-}

{- LIMA_INDENT 4 -}

-- >>>("hit the road, jack" :: String) & worded %~ take 3 & worded %~ drop 2
-- "t e a c"

-- >>>("hit the road, jack" :: String) & worded %~ (take 3 . drop 2)
-- "t e ad, ck"

{-
2. Break the Law One
-}

myTraversal :: Traversal Int Int Int Int
myTraversal f _ = f 1

-- >>>(traverseOf myTraversal pure 6) :: Identity Int
-- Identity 1

-- >>>pure 6 :: Identity Int
-- Identity 6

{-
3. Break the Law Two
-}

ex60 :: Traversal' [Int] Int
ex60 = traversed . filtered even

-- >>> [1, 2, 3] & ex60 %~ (+ 1) & ex60 %~ (+ 2)
-- [1,3,3]

-- >>> [1, 2, 3] & ex60 %~ (+ 1) . (+ 2)
-- [1,5,3]

{-
4. Check lawful

- `taking` is lawful
- `beside` is lawful
- `each` is lawful
- `lined` is unlawful
- `traversed` is lawful
-}

{- LIMA_INDENT 2 -}

-- >>>("hit\nthe\nroad,\njack" :: String) & lined %~ take 3 & lined %~ drop 2
-- "t\ne\na\nc"

-- >>>("hit\nthe\nroad,\njack" :: String) & lined %~ (take 3 . drop 2)
-- "t\ne\nad,\nck"

{-
update function can insert newlines
-}

-- >>>("hit\nthe\nroad,\njack" :: String) & lined %~ (\(x:y:xs) -> (x:y:'\n':xs)) & lined %~ take 2
-- "hi\nt\nth\ne\nro\nad\nja\nck"

-- >>>("hit\nthe\nroad,\njack" :: String) & lined %~ (take 2 . \(x:y:xs) -> (x:y:'\n':xs))
-- "hi\nth\nro\nja"

{- LIMA_DEDENT -}

{-
### 7.7 Advanced manipulation

#### partsOf

Real:
-}

-- >>>:t partsOf
-- partsOf :: Functor f => Traversing (->) f s t a a -> LensLike f s t [a] [a]

{-
Book:

- Make a lens whose focuses are focuses of a provided traversal

    ```hs
    partsOf :: Traversal' s a -> Lens' s [a]
    ```
-}

-- >>> [('a', 1 :: Int), ('b', 2), ('c', 3)] & partsOf (traversed . _2) .~ [4]
-- [('a',4),('b',2),('c',3)]

-- >>> [('a', 1 :: Int), ('b', 2), ('c', 3)] & partsOf (traversed . _2) .~ [4,5,6,7,8]
-- [('a',4),('b',5),('c',6)]

{-
Cool example:

1. focus all characters in strings
1. concatenate, split into words, sort words, concatenate back
1. place on corresponding places
-}

-- >>> ("how is a raven ", "like a ", "writing desk") & partsOf (each . traversed) %~ unwords . sort . words
-- ("a a desk how is"," like r","aven writing")

{-
Placement matters
-}

-- Collect 'each' tuple element into a list, then traverse that list
-- >>> ("abc", "def") ^.. partsOf each . traversed
-- ["abc","def"]

-- Collect each tuple element, then traverse those strings collecting each character into a list.
-- >>> (("abc", "def") ^.. partsOf (each . traversed)) :: [String]
-- ["abcdef"]

{-
Can use other focuses for calculating each
-}

ex61 :: [(Char, Double)]
ex61 =
  [('a', 1), ('b', 2), ('c', 3)]
    & partsOf (traversed . _2)
      %~ \xs -> (/ sum xs) <$> xs

-- >>>ex61
-- [('a',0.16666666666666666),('b',0.3333333333333333),('c',0.5)]

{-
#### Polymorphic partsOf

We can change type of focuses if supply enough elements

```hs
unsafePartsOf :: Traversal s t a b -> Lens s t [a] [b]
```
-}

-- >>>[('a', 1), ('b', 2), ('c', 3)] & unsafePartsOf (traversed . _1) .~ [True, False]
-- unsafePartsOf': not enough elements were supplied

ex62 :: [((Char, Maybe Char), Integer)]
ex62 =
  [('a', 1), ('b', 2), ('c', 3)]
    & unsafePartsOf (traversed . _1)
      %~ \xs -> zip xs ((Just <$> tail xs) ++ [Nothing])

-- >>>ex62
-- [(('a',Just 'b'),1),(('b',Just 'c'),2),(('c',Nothing),3)]

{-
#### partsOf and other data structures

Replace each ID in a Tree with a User

```hs
userIds :: Tree UserId
lookupUsers :: [UserId] -> IO [User]
treeLookup :: Tree UserId -> IO (Tree User)
treeLookup = traverseOf (unsafePartsOf traversed) lookupUsers
```

#### Exercises - partsOf
-}

-- >>> [1, 2, 3, 4] ^. partsOf (traversed . filtered even)
-- [2,4]

-- >>> ["Aardvark" :: String, "Bandicoot", "Capybara"] ^. traversed . partsOf (taking 3 traversed)
-- "AarBanCap"

ex63 :: [Int]
ex63 = ([1, 2], M.fromList [('a', 3), ('b', 4)]) ^. partsOf (beside traversed traversed)

-- >>> ex63
-- [1,2,3,4]

-- >>> [1, 2, 3, 4] & partsOf (traversed . filtered even) .~ [20, 40]
-- [1,20,3,40]

-- >>> ["Aardvark", "Bandicoot", "Capybara"] & partsOf (traversed . traversed) .~ "Kangaroo"
-- ["Kangaroo","Bandicoot","Capybara"]

-- >>> ["Aardvark", "Bandicoot", "Capybara"] & partsOf (traversed . traversed) .~ "Ant"
-- ["Antdvark","Bandicoot","Capybara"]

-- Modifying
-- Tip: Map values are traversed in order by KEY
-- >>> M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')] & partsOf traversed %~ \(x:xs) -> xs ++ [x]
-- fromList [('a','b'),('b','c'),('c','a')]

-- >>> ('a', 'b', 'c') & partsOf each %~ reverse
-- ('c','b','a')

-- >>> [1, 2, 3, 4, 5, 6] & partsOf (taking 3 traversed) %~ reverse
-- [3,2,1,4,5,6]

-- >>> ('a', 'b', 'c') & unsafePartsOf each %~ \xs -> fmap ((,) xs) xs
-- (("abc",'a'),("abc",'b'),("abc",'c'))

{-
## 8. Indexable Structures

### 8.1 What’s an “indexable” structure?

<b>Indexable</b> structures store values at <b>named locations</b> which can be identified by some <b>index</b>.
That is, an <b>index</b> represents a <b>specific location</b> within a data structure where a value <b>might</b> be stored.

Data structures have different interfaces (lists, dicts)

### 8.2 Accessing and updating values with ‘Ixed’

#### The Ixed Class

Unifies the interface to all data structures.

```hs
class Ixed m where
  ix :: Index m -> Traversal' m (IxValue m)
```

makes a Traversal because an Index at a specified location may be missing.

These are Type Families that calculate an index an a value types for a data structure.

```hs
type instance Index [a] = Int
type instance IxValue [a] = a

type instance Index (Map k a) = k
type instance IxValue (Map k a) = a

type instance Index Text = Int
type instance IxValue Text = Char

type instance Index ByteString = Int
type instance IxValue ByteString = Word8
```

#### Accessing and setting values with ix

Can't add or remove focuses.

Lists:
-}

humanoids :: [String]
humanoids = ["Borg", "Cardassian", "Talaxian"]

-- >>> -- Get the value at index 1:
-- >>> humanoids & ix 1 .~ "Vulcan"
-- ["Borg","Vulcan","Talaxian"]
-- >>> -- There's no value at index 10 so the traversal doesn't focus anything
-- >>> humanoids & ix 10 .~ "Romulan"
-- ["Borg","Cardassian","Talaxian"]

{-
Maps:
-}

benders :: M.Map String String
benders = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]

-- Get the value at key "Zuko"
-- >>> benders ^? ix "Zuko"
-- Just "Fire"

-- If there's no value at a key, the traversal returns zero elements
-- >>> benders ^? ix "Sokka"
-- Nothing

-- We can set the value at a key, but only if that key already exists
-- >>> benders & ix "Toph" .~ "Metal"
-- fromList [("Katara","Water"),("Toph","Metal"),("Zuko","Fire")]

-- Setting a non-existent element of a Map does NOT insert it.
-- >>> benders & ix "Iroh" .~ "Lightning"
-- fromList [("Katara","Water"),("Toph","Earth"),("Zuko","Fire")]

{-
#### Indexed Structures
-}

-- >>> :kind! forall a. Index [a]
-- forall a. Index [a] :: *
-- = Int

-- >>> :kind! forall a. IxValue [a]
-- forall a. IxValue [a] :: *
-- = a

{-
#### Indexing monomorphic types
-}

-- >>>("hello" :: T.Text) ^? ix 0
-- Just 'h'

-- We can edit a Word8 within a ByteString as though it's an integer.
-- >>> ("hello" :: BS.ByteString) & ix 0 +~ 2
-- "jello"

{-
Cool example:
-}

ex64 :: [T.Text]
ex64 = ("hello" :: T.Text) & ix 1 %%~ const ("aeiou" :: [Char])

{-
Explanation:

```hs
type instance IxValue [a] = a
instance Ixed [a] where
  ix k f xs0 | k < 0     = pure xs0
             | otherwise = go xs0 k where
    go [] _ = pure []
    go (a:as) 0 = f a <&> (:as)
    go (a:as) i = (a:) <$> (go as $! i - 1)
  {-# INLINE ix #-}
```

So, we'll pre- and append the not-focused parts inside the Functorial context.
-}

ex64' :: [String]
ex64' = ('h' :) <$> (const "aeiou" 'e' <&> (: "llo"))

-- >>>ex64'
-- ["hallo","hello","hillo","hollo","hullo"]

{-
#### Indexing stranger structures

Numbers denote node children
-}

tree :: Tree Int
tree = Node 1 [Node 2 [Node 4 []], Node 3 [Node 5 [], Node 6 []]]

-- >>> tree ^? ix [1, 1]
-- Just 6

-- >>> tree ^? ix [5, 6]
-- Nothing

{-
Functions:

> We can "set" or traverse individual results of a function!
Here we overwrite the function's output at the input value "password"
so it instead returns a new value.
-}

-- >>> myPass = (reverse & ix "password" .~ "You found the secret!")
-- >>> "pass" & myPass
-- "ssap"
-- >>> "password" & myPass
-- "You found the secret!"

{-
### 8.3 Inserting & Deleting with ‘At’

#### Map-like structures

Can be used with structures that support inserts by an arbitrary index.

- `Map k v`
- `Set k` (~ `Map k ()`)
Lists don't support that. E.g., can't insert 10th element without having 9th.

```hs
class At where
  at :: Index m -> Lens' m (Maybe (IxValue m))

ix :: Index m -> Traversal' m (IxValue m)
at :: Index m -> Lens' m (Maybe (IxValue m))

(?~) :: Traversal s t a (Maybe b) -> b -> s -> t
```
-}

-- >>>benders & at "Iroh" ?~ "Lightning"
-- fromList [("Iroh","Lightning"),("Katara","Water"),("Toph","Earth"),("Zuko","Fire")]

{-
```hs
sans :: At m => Index m -> m -> m
sans k = at k .~ Nothing
```
-}

-- >>> sans "Katara" benders
-- fromList [("Toph","Earth"),("Zuko","Fire")]

ps :: [Int]
ps = foldl (\acc x -> acc <> check acc x) [2] [3 .. 100]
 where
  check (a : as) x
    | a * a > x = [x]
    | x `mod` a == 0 = []
    | otherwise = check as x
  check [] x = [x]

-- >>> ps
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

primes :: S.Set Int
primes = S.fromList (ps ^.. taking 5 traversed)

-- >>> primes & at 17 ?~ ()
-- fromList [2,3,5,7,11,17]

{-
#### Exercises – Indexable Structuresm

1. fill in blanks
-}

-- >>> ["Larry", "Curly", "Moe"] & ix 1 .~ "Wiggly"
-- ["Larry","Wiggly","Moe"]

heroesAndVillains :: M.Map String String
heroesAndVillains = M.fromList [("Superman", "Lex"), ("Batman", "Joker")]

-- >>> heroesAndVillains & at "Spiderman" .~ Just "Goblin"
-- fromList [("Batman","Joker"),("Spiderman","Goblin"),("Superman","Lex")]

-- >>> sans "Superman" heroesAndVillains
-- fromList [("Batman","Joker")]

-- >>> S.fromList ['a', 'e', 'i', 'o', 'u'] & at 'y' .~ Just () & at 'i' .~ Nothing
-- fromList "aeouy"

{-
2. input -> output
-}

input :: M.Map String Integer
input = M.fromList [("candy bars", 13), ("gum", 7), ("soda", 34)]

output :: M.Map String Integer
output = M.fromList [("candy bars", 13), ("ice cream", 5), ("soda", 37)]

-- >>> input & at "soda" %~ ((+ 3) <$>) & sans "gum" & at "ice cream" ?~ 5
-- fromList [("candy bars",13),("ice cream",5),("soda",37)]

{-
## 10. Isos

- isomorphism - a completely <b>reversible transformation</b> between two types or formats.
- every iso MUST succeed for all inputs.

![isos](./README/tableIsos.png)

Example: converting `Text` to `String`:

```hs
T.pack . T.unpack = id
T.unpack . T.pack = id
```

Construct an `Iso`:

```hs
iso :: (s -> a) -> (b -> t) -> Iso s t a b
```
-}

packed :: Iso' String T.Text
packed = iso to' from'
 where
  to' :: String -> T.Text
  to' = T.pack
  from' :: T.Text -> String
  from' = T.unpack

-- >>> ("Ay, caramba!" :: String) ^. packed
-- "Ay, caramba!"

-- Use isos as prisms
-- >>> packed # ("Sufferin' Succotash" :: T.Text)
-- "Sufferin' Succotash"

{-
### 10.3 Flipping isos with from

```hs
from :: Iso s t a b -> Iso b a t s
from :: Iso' s a -> Iso' a s
```
-}

-- >>> ("Good grief" :: T.Text) ^. from packed
-- "Good grief"

{-
Reversing again.

```hs
unpacked :: Iso' T.Text String
unpacked = from packed
```

### 10.4 Modification under isomorphism

Example: focus on `Text` (to use functions existing for `Text`), then convert back to a `String`.
-}

-- >>> let str = "Idol on a pedestal" :: String
-- >>> over packed (T.replace "Idol" "Sand") str
-- "Sand on a pedestal"

-- Combining with other optics
-- >>> import Data.Char (toUpper)
-- >>> let txt = "Lorem ipsum" :: T.Text
-- >>> txt & from packed . traversed %~ toUpper
-- "LOREM IPSUM"

{-
### 10.5 Varieties of isomorphisms

Isos for the same type

```hs
reversed :: Iso' [a] [a]
reversed = iso reverse reverse

involuted :: (a -> a) -> Iso' a a
involuted f = iso f f

reversed :: Iso' [a] [a]
reversed = involuted reverse
```
-}

-- >>> "Blue suede shoes" & reversed . taking 1 worded . reversed .~ "gloves"
-- "Blue suede gloves"

{-
Rearrange pairs

```hs
swapped :: Iso (s, s') (t, t') (a, a') (b, b')

swapped :: (Bifunctor p, Swapped p) => Iso (p a b) (p c d) (p b a) (p d c)
```
-}

-- >>> ("Fall","Pride") ^. swapped
-- ("Pride","Fall")

-- >>> Right "Field" ^. swapped
-- Left "Field"

{-
Isos for functions

```hs
flipped :: Iso' (a -> b -> c) (b -> a -> c)
```
-}

-- >>> let (++?) = (++) ^. flipped
-- >>> "A" ++? "B"
-- "BA"

{-
more

```hs
curried :: Iso' ((a, b) -> c) (a -> b -> c)
uncurried :: Iso' (a -> b -> c) ((a, b) -> c)
```
-}

-- >>> let addTuple = (+) ^. uncurried
-- >>> addTuple (1, 2)
-- 3

{-
Isos for numbers
-}

-- >>> 100 ^. adding 50
-- 150

{-
#### Composing isos
-}

-- >>> import Numeric.Lens
-- >>> 30 & dividing 10 . multiplying 2 +~ 1
-- 35.0

-- 30 -> 30/10 = 3 -> 3 * 2 = 6 -> 6 + 1 = 7 -> 7 / 2 = 3.5 -> 3.5 * 10 = 35

{-
#### Exercises – Intro to Isos

1. Choose the best optic:
  - Focus a Celsius temperature in Fahrenheit - Iso - reversible
  - Focus the last element of a list - Traversal - the element may be missing
  - View a JSON object as its corresponding Haskell Record - Prism - may fail to parse
  - Rotate the elements of a three-tuple one to the right - Iso - rotation is reversible
  - Focus on the ‘bits’ of an Int as Bools - Traversal or Prism - multiple focuses
  - Focusing an IntSet from a Set Int - Iso - reversible

1. Fill in the blank
-}

-- >>> ("Beauty", "Age") ^. swapped
-- ("Age","Beauty")

-- >>> 50 ^. adding 10
-- 60

-- >>> 50 ^. from (adding 10)
-- 40

-- >>> 0 & multiplying 4 +~ 12
-- 3.0

-- >>> 0 & adding 10 . multiplying 2 .~ _
-- 2

-- Note: transpose flips the rows and columns of a nested list:
-- >>> import Data.List (transpose)
-- >>> transpose [[1, 2, 3], [10, 20, 30]]
-- [[1,10],[2,20],[3,30]]
-- >>> [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ drop 1
-- [[2,3],[20,30]]

-- Extra hard: use `switchCase` somehow to make this statement work:
ex65 :: (Integer, String)
ex65 = (32, "Hi") & _2 . involuted (map switchCase) .~ ("hELLO" :: String)
 where
  switchCase c = if isUpper c then toLower c else toUpper c

-- >>> ex65
-- (32,"Hello")

{-
3. Conversion
-}

celsiusToF :: Double -> Double
celsiusToF c = (c * (9 / 5)) + 32

fToCelsius :: Double -> Double
fToCelsius f = (f - 32) * 5 / 9

fahrenheit' :: Iso' Double Double
fahrenheit' = iso fToCelsius celsiusToF

-- >>> 0 & fahrenheit' .~ 100
-- 212.0

{-
### 10.6 Projecting Isos

We can lift Isos into other structures.

```hs
mapping :: (Functor f, Functor g) => Iso s t a b -> Iso (f s) (g t) (f a) (g b)
```
-}

toYamlList :: [String] -> String
toYamlList xs = "- " <> intercalate "\n- " xs

shoppingList :: [T.Text]
shoppingList = ["Milk", "Eggs", "Flour"] :: [T.Text]

-- >>> shoppingList ^. mapping unpacked . to toYamlList
-- "- Milk\n- Eggs\n- Flour"

{-
There's more:

```hs
contramapping :: Contravariant f => Iso s t a b -> Iso (f a) (f b) (f s) (f t)
bimapping :: (Bifunctor f, Bifunctor g) => Iso s t a b -> Iso s' t' a' b' -> Iso (f s s') (g t t') (f a a') (g b b')
dimapping :: (Profunctor p, Profunctor q) => Iso s t a b -> Iso s' t' a' b' -> Iso (p a s') (q b t') (p s a') (q t b')
```
-}

textToYamlList :: [T.Text] -> T.Text
textToYamlList = (toYamlList :: [String] -> String) ^. dimapping (mapping unpacked :: Iso' [T.Text] [String]) (packed :: Iso' String T.Text)

-- much more readable
textToYamlList' :: [T.Text] -> T.Text
textToYamlList' = T.pack . toYamlList . fmap T.unpack

{-
#### Exercises – Projected Isos

1. Fill in the blank
-}

{- LIMA_INDENT 4 -}

-- >>> ("Beauty", "Age") ^. mapping reversed . swapped
-- ("egA","Beauty")

-- >>> [True, False, True] ^. mapping (involuted not)
-- [False,True,False]

-- >>> [True, False, True] & mapping (involuted not) %~ filter id
-- [False]

-- >>> (show ^. mapping reversed) 1234
-- "4321"

{-
2. Using `enum :: Enum a => Iso' Int a` implement the `intNot`.
-}

intNot :: Int -> Int
intNot = not ^. dimapping enum (from enum)

-- >>> intNot 0
-- 1

-- >>> intNot 1
-- 0

-- >>> intNot 2
-- Prelude.Enum.Bool.toEnum: bad argument

intNot' :: Int -> Int
intNot' = fromEnum . not . toEnum @Bool

-- >>> intNot' 0
-- 1

-- >>> intNot' 1
-- 0

-- >>> intNot' 2
-- Prelude.Enum.Bool.toEnum: bad argument

{-
### 10.7 Isos and newtypes

#### Coercing with isos

- Coercible is derived for newtypes by the compiler
- Can coerce between newtypes

```
coerced :: (Coercible s a, Coercible t b) => Iso s t a b
```
-}

newtype Email = Email {_email :: String} deriving (Show)

-- >>> Email "hi\nu"
-- Email {_email = "hi\nu"}

-- >>> over coerced (reverse :: String -> String) (Email "joe@example.com") :: Email
-- Email {_email = "moc.elpmaxe@eoj"}

email :: Iso' Email String
email = coerced

ex66 :: String
ex66 = Email "joe@example.com" ^. email . reversed

{-
#### Newtype wrapper isos

- `makeLenses` derives isos

```hs
_Wrapped' :: Wrapped s => Iso' s (Unwrapped s)
_Unwrapped' :: Wrapped s => Iso' (Unwrapped s) s
```

- map <b>only</b> between types and their newtype wrappers.
- can be generated via `makeWrapped`
-}

makeWrapped ''Email

ex67 :: Email
ex67 = Email "joe@example.com" & _Wrapped' @Email %~ reverse

-- >>> ex67
-- Email {_email = "moc.elpmaxe@eoj"}

{-
### 10.8 Laws

### Reversibility

```hs
myIso . from myIso == id
from myIso . myIso == id
```
-}

-- >>> view (from reversed . reversed) ("Testing one two three")
-- "Testing one two three"

{-
#### Exercises – Iso Laws

1. The following iso is unlawful; provide a counter example which shows that it breaks the law.
-}

mapList :: Ord k => Iso' (M.Map k v) [(k, v)]
mapList = iso M.toList M.fromList

kvInts :: [(Int, Int)]
kvInts = [(2 :: Int, 1 :: Int), (1, 2)]

ex68 :: [(Int, Int)]
ex68 = kvInts ^. from mapList . mapList

-- >>> ex68
-- [(1,2),(2,1)]

-- >>> ex68 == kvInts
-- False

{-
2. Is there a lawful implementation of the following iso? If so, implement it, if not, why not?

Yes, there is one.
-}

nonEmptyList :: Iso [a] [b] (Maybe (NonEmpty a)) (Maybe (NonEmpty b))
nonEmptyList = iso nonEmpty (maybe [] Data.List.NonEmpty.toList)

-- >>> [] ^. nonEmptyList . from nonEmptyList
-- []

-- >>> Nothing ^. from nonEmptyList . nonEmptyList
-- Nothing

-- >>> [1] ^. nonEmptyList . from nonEmptyList
-- [1]

-- >>> (Just (1 :| [])) ^. from nonEmptyList . nonEmptyList
-- Just (1 :| [])

{-
3. Is there a lawful implementation of an iso which ‘sorts’ a list of elements? If so, implement it, if
not, why not?

```hs
sorted :: Ord a => Iso' [a] [a]
```

There's no implementation for this iso because it's impossible to unsort a sorted list.

4. What about the following iso which pairs each element with an Int which remembers its original
position in the list. Is this a lawful iso? Why or why not? If not, try to find a counter-example.
-}

sorted :: (Ord a) => Iso' [a] [(Int, a)]
sorted = iso to' from'
 where
  to' xs = L.sortOn snd $ zip [0 ..] xs
  from' xs = snd <$> L.sortOn fst xs

-- >>> [2, 1] ^. sorted . from sorted
-- [2,1]

-- >>> [(1, 1), (0, 2)] ^. from sorted . sorted
-- [(1,1),(0,2)]


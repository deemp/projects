{-
# Optics By Example
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
{- LIMA_DISABLE -}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{- LIMA_ENABLE -}
{- FOURMOLU_ENABLE -}

module Lib (someFunc) where

import Control.Lens

import Control.Applicative
import Control.Lens
import Control.Lens.Unsound (lensProduct)
import Data.Char
import Data.Foldable (Foldable (..))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Language.Haskell.TH (Dec, Q, Quote, runQ)
import Language.Haskell.TH.Syntax (Quasi)

someFunc :: IO ()
someFunc = print "hello"

{-
## 3. Lenses

- A Lens focuses (i.e. selects) a single piece of data within a larger structure.
- A Lens must never fail to get or modify that focus.

### 3.1 Introduction to Lenses

#### Exercises - Optic Anatomy

Find: action, path, structure, focus
-}

-- >>> view (_1 . _2) ((1, 2), 3)
-- 2

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

getName :: Ship -> String
getName = _name

setName :: Ship -> String -> Ship
setName ship _name = ship{_name}

name_ :: Lens' Ship String
name_ = lens getName setName

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
##### Exercises - Records Part Two
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
1. Setting back what you got doesn’t do anything (get-set)
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

In `Lens s t a b`:

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
##### Exercises – Polymorphic Lenses
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
#### How do I update fields in deeply nested records?
-}

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Try.TypeFamilies.TaggedClasses where
```

# Tagged classes

- [tagged-aeson](https://github.com/monadfix/tagged-aeson)

## Instance Chains

PureScript provides [Instance Chains](https://github.com/purescript/documentation/blob/master/language/Type-Classes.md#instance-chains).

Here's how they can be rewritten using Tagged Classes

source - [purescript-barlow-lens](https://github.com/sigma-andex/purescript-barlow-lens/blob/295c4b32fbeca052ebfd3665a9071012e654b9c0/src/Data/Lens/Barlow/Parser.purs#L11)

```haskell
import Data.Kind (Constraint)
import GHC.TypeLits (AppendSymbol, Symbol)

data Tag
  = WhenDot
  | WhenSpace
  | WhenSame
  | Else

type SelectTag :: Symbol -> Symbol -> Symbol -> Symbol -> Tag
type family SelectTag head tail out rest where
  SelectTag "." t "." t = WhenDot
  SelectTag " " t "" t = WhenSpace
  SelectTag h "" h "" = WhenSame
  SelectTag _ _ _ _ = Else

type ParsePercentageSymbol' :: Tag -> Symbol -> Symbol -> Symbol -> Symbol -> Constraint
class ParsePercentageSymbol' tag head tail out rest

instance ParsePercentageSymbol' WhenDot "." t "" t
instance ParsePercentageSymbol' WhenSpace " " t "" t
instance ParsePercentageSymbol' WhenSame h "" h ""
instance (ParsePercentageSymbol th tt tout trest, t ~ AppendSymbol th tt, out ~ AppendSymbol h tout) => ParsePercentageSymbol' Else h t out trest

type ParsePercentageSymbol :: Symbol -> Symbol -> Symbol -> Symbol -> Constraint
type ParsePercentageSymbol h t o r = ParsePercentageSymbol' (SelectTag h t o r) h t o r
```

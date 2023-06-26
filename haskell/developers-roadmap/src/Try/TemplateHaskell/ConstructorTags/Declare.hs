{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Try.TemplateHaskell.ConstructorTags.Declare where

import Control.Lens (Iso', iso, (&), (<>~))
import Control.Monad
import Language.Haskell.TH

data HydraEvent
  = GetUTxOResponse Int
  | TxValid Int

p1 :: Q [Dec]
p1 =
  [d|
    data HydraEvent
      = GetUTxOResponse Int
      | TxValid Int
    |]

-- >>> runQ p1
-- [DataD [] HydraEvent_159 [] Nothing [NormalC GetUTxOResponse_160 [(Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Int)],NormalC TxValid_161 [(Bang NoSourceUnpackedness NoSourceStrictness,ConT GHC.Types.Int)]] []]

data HydraEventKind
  = GetUTxOResponseKind
  | TxValidKind
  deriving stock (Eq, Show)

p2 :: Q [Dec]
p2 =
  [d|
    data HydraEventKind
      = GetUTxOResponseKind
      | TxValidKind
      deriving stock (Eq, Show)
    |]

-- >>> runQ p2
-- [DataD [] HydraEventKind_156 [] Nothing [NormalC GetUTxOResponseKind_157 [],NormalC TxValidKind_158 []] [DerivClause (Just StockStrategy) [ConT GHC.Classes.Eq,ConT GHC.Show.Show]]]

nm :: Iso' Name String
nm = iso from to
 where
  from = nameBase
  to = mkName

deriveTags :: Name -> String -> [Name] -> Q [Dec]
deriveTags ty suff classes = do
  (TyConI tyCon) <- reify ty
  (tyName, cs) <- case tyCon of
    DataD _ n _ _ cs _ -> pure (n, cs)
    NewtypeD _ n _ _ cs _ -> pure (n, [cs])
    _ -> fail "deriveTags: only 'data' and 'newtype' are supported"
  cs' <-
    forM
      cs
      ( let mk n = pure $ NormalC (n & nm <>~ suff) []
         in \case
              NormalC n _ -> mk n
              RecC n _ -> mk n
              _ -> fail "deriveTags: constructor names must be NormalC or RecC (See https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/src/Language.Haskell.TH.Syntax.html#Con)"
      )
  let v = DataD [] (tyName & nm <>~ suff) [] Nothing cs' [DerivClause (Just StockStrategy) (ConT <$> classes)]
  pure [v]

getHydraEventKind :: HydraEvent -> HydraEventKind
getHydraEventKind event = case event of
  GetUTxOResponse{} -> GetUTxOResponseKind
  TxValid{} -> TxValidKind

p3 :: Q [Dec]
p3 =
  [d|
    getHydraEventKind :: HydraEvent -> HydraEventKind
    getHydraEventKind event = case event of
      GetUTxOResponse{} -> GetUTxOResponseKind
      TxValid{} -> TxValidKind
    |]

-- >>> runQ p3
-- [SigD getHydraEventKind_168 (AppT (AppT ArrowT (ConT Try.TemplateHaskell.Declare.HydraEvent)) (ConT Try.TemplateHaskell.Declare.HydraEventKind)),FunD getHydraEventKind_168 [Clause [VarP event_169] (NormalB (CaseE (VarE event_169) [Match (RecP Try.TemplateHaskell.Declare.GetUTxOResponse []) (NormalB (ConE Try.TemplateHaskell.Declare.GetUTxOResponseKind)) [],Match (RecP Try.TemplateHaskell.Declare.TxValid []) (NormalB (ConE Try.TemplateHaskell.Declare.TxValidKind)) []])) []]]

deriveMapping :: Name -> String -> String -> Q [Dec]
deriveMapping ty suff mappingName = do
  (TyConI tyCon) <- reify ty
  (tyName, cs) <- case tyCon of
    DataD _ n _ _ cs _ -> pure (n, cs)
    NewtypeD _ n _ _ cs _ -> pure (n, [cs])
    _ -> fail "deriveTags: only 'data' and 'newtype' are supported"
  let
    sig = SigD (mkName mappingName) (AppT (AppT ArrowT (ConT ty)) (ConT (tyName & nm <>~ suff)))
  event <- newName "event"
  matches <-
    forM
      cs
      ( let mk n = pure $ Match (RecP n []) (NormalB (ConE (n & nm <>~ suff))) []
         in \case
              NormalC n _ -> mk n
              RecC n _ -> mk n
              _ -> fail "deriveTags: constructor names must be NormalC or RecC (See https://hackage.haskell.org/package/template-haskell-2.20.0.0/docs/src/Language.Haskell.TH.Syntax.html#Con)"
      )
  let
    fun = FunD (mkName mappingName) [Clause [VarP event] (NormalB (CaseE (VarE event) matches)) []]
  pure [sig, fun]

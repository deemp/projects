{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Try.Aeson.HKD where

import Control.Lens (Identity, non, (&), (^.))
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Kind
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Fcf (Eval, Exp)
import GHC.Generics

data ORBEParty f = ORBEParty
  { infants :: Eval (f Int)
  , children :: Eval (f Int)
  , adults :: Eval (f Int)
  }
  deriving (Generic)

data MyIdentity :: a -> Exp a
data MyMaybe :: a -> Exp a

type instance Eval (MyIdentity a) = a
type instance Eval (MyMaybe a) = Maybe (NonEmpty a)

instance FromJSON (ORBEParty MyMaybe)
deriving instance Show (ORBEParty MyIdentity)
deriving instance Show (ORBEParty MyMaybe)

t1 = "{ \"adults\": [3] }" & decode @(ORBEParty MyMaybe)

-- >>> t1
-- Just (ORBEParty {infants = Nothing, children = Nothing, adults = Just (3 :| [])})

toDefault :: ORBEParty MyMaybe -> ORBEParty MyIdentity
toDefault ORBEParty{..} =
  ORBEParty
    { infants = maybe 1 NE.head infants
    , children = maybe 1 NE.head children
    , adults = maybe 1 NE.head adults
    }

data Party = Party
  { adults :: Maybe Int
  , children :: Maybe (NonEmpty Int)
  , infants :: Maybe Int
  }
  deriving (Generic, Show)

deriveJSON defaultOptions ''Party

t = "{ \"adults\": 3, \"children\" : [] }" & decode @Party

-- >>> t
-- Nothing

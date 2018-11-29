{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Validation.Id where

import GHC.Generics (Generic)

import Control.Lens

import Data.Text (Text)

import Data.Aeson (ToJSON, FromJSON)

data Id = Id {
    _idParent :: Maybe Id
  , _idTag :: Text
  }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''Id

instance ToJSON Id where
instance FromJSON Id where

idApp :: Text -> Id -> Id
idApp t i = Id (Just i) t

idToText :: Id -> Text
idToText (Id mi t) = maybe ""  idToText mi <> t

matchOrDescendant :: Id -> Id -> Bool
matchOrDescendant i1 i2 =
  i1 == i2 ||
  maybe False (matchOrDescendant i1) (view idParent i2)

data WithId a = WithId { _wiId :: Id, _wiValue :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic)

makeLenses ''WithId

instance ToJSON a => ToJSON (WithId a) where
instance FromJSON a => FromJSON (WithId a) where

hasMatchingId :: Id -> [WithId e] -> Bool
hasMatchingId i = any ((== i) . view wiId)

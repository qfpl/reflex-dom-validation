{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MonoLocalBinds #-}
module Demo.Example (
    go
  ) where

import Data.Functor.Identity (Identity(..))

import Reflex.Dom.Core

import Reflex.Dom.Validation
import Bootstrap

import Data.Functor.Identity (Identity(..))
import Data.Dependent.Map (Some(..))
import Data.Dependent.Sum (ShowTag(..))
import Data.GADT.Show
import Data.GADT.Compare
import Data.GADT.Aeson
import Data.Aeson (ToJSON(..), FromJSON(..))

import Demo.Example.CompletedWithReason
import Demo.Example.TestCollections
import Demo.Example.Workflow
import Demo.Example.Error

-- TODO do we always want changes to be saved?
-- - at the moment, any change causes the main dynamic to change
-- - we might want to tie that to buttons in workflows etc..., and so making that configurable might be a thing

data ExampleTag a where
  ExTag :: ExampleTag (TestCollections Maybe)

instance GEq ExampleTag where
  geq ExTag ExTag = Just Refl

instance GCompare ExampleTag where
  gcompare ExTag ExTag = GEQ

instance GShow ExampleTag where
  gshowsPrec _p ExTag = showString "Tag"

instance ShowTag ExampleTag Identity where
  showTaggedPrec ExTag = showsPrec

instance GKey ExampleTag where
  toKey (This ExTag) = "tag"
  fromKey t =
    case t of
      "tag" -> Just (This ExTag)
      _ -> Nothing
  keys _ = [This ExTag]

instance ToJSONTag ExampleTag Identity where
  toJSONTagged ExTag (Identity x) = toJSON x

instance FromJSONTag ExampleTag Identity where
  parseJSONTagged ExTag x = Identity <$> parseJSON x

go :: forall t m. MonadWidget t m => m (Event t (TestCollections Identity))
go =
  wrapUpStorage (testCollectionsF :: Field t m MyError TestCollections TestCollections) ExTag mempty $ \d -> do
    -- the version with a validation button
    eValidate <- buttonClass "Validate" "btn"
    pure (current d <@ eValidate)

    -- the version where every change causes a validation
    -- pure (updated d)

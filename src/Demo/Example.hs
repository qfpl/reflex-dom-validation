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

import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString.Lazy as LBS

import Reflex.Dom.Validation
import Bootstrap

import Data.Functor.Identity (Identity(..))
import Data.Dependent.Map (Some(..))
import Data.Dependent.Sum (ShowTag(..))
import Data.GADT.Show
import Data.GADT.Compare
import Data.GADT.Aeson
import Data.Aeson (encode, ToJSON(..), FromJSON(..))

import Demo.Example.CompletedWithReason
import Demo.Example.TestCollections
import Demo.Example.Workflow
import Demo.Example.Error

-- TODO do we always want changes to be saved?
-- - at the moment, any change causes the main dynamic to change
-- - we might want to tie that to buttons in workflows etc..., and so making that configurable might be a thing

data ExampleTag a where
  DataTag :: ExampleTag (TestCollections Maybe)
  UiTag :: ExampleTag TestCollectionsU
  ErrorsTag :: ExampleTag [WithId MyError]

instance GEq ExampleTag where
  geq DataTag DataTag = Just Refl
  geq UiTag UiTag = Just Refl
  geq ErrorsTag ErrorsTag = Just Refl
  geq _ _ = Nothing

instance GCompare ExampleTag where
  gcompare DataTag DataTag = GEQ
  gcompare DataTag _ = GLT
  gcompare _ DataTag = GGT
  gcompare UiTag UiTag = GEQ
  gcompare UiTag _ = GLT
  gcompare _ UiTag = GGT
  gcompare ErrorsTag ErrorsTag = GEQ

instance GShow ExampleTag where
  gshowsPrec _p DataTag = showString "Data"
  gshowsPrec _p UiTag = showString "Ui"
  gshowsPrec _p ErrorsTag = showString "Error"

instance ShowTag ExampleTag Identity where
  showTaggedPrec DataTag = showsPrec
  showTaggedPrec UiTag = showsPrec
  showTaggedPrec ErrorsTag = showsPrec

instance GKey ExampleTag where
  toKey (This DataTag) = "data"
  toKey (This UiTag) = "ui"
  toKey (This ErrorsTag) = "error"

  fromKey t =
    case t of
      "data" -> Just (This DataTag)
      "ui" -> Just (This UiTag)
      "error" -> Just (This ErrorsTag)
      _ -> Nothing
  keys _ = [This DataTag, This UiTag, This ErrorsTag]

instance ToJSONTag ExampleTag Identity where
  toJSONTagged DataTag (Identity x) = toJSON x
  toJSONTagged UiTag (Identity x) = toJSON x
  toJSONTagged ErrorsTag (Identity x) = toJSON x

instance FromJSONTag ExampleTag Identity where
  parseJSONTagged DataTag x = Identity <$> parseJSON x
  parseJSONTagged UiTag x = Identity <$> parseJSON x
  parseJSONTagged ErrorsTag x = Identity <$> parseJSON x

go :: forall t m. MonadWidget t m => m (Event t (TestCollections Identity))
go =
  let
    tc = testCollectionsF :: Field t m MyError TestCollections TestCollections TestCollectionsU TestCollectionsU
    tcu = TestCollectionsU (FooU 0 (NestU 0 (Nest1U 0) (Nest2U 0) (Nest3U 0))) mempty
  in
    wrapUpStorage tc DataTag mempty UiTag tcu ErrorsTag $ \d -> divClass "row" $ do
      -- the version with a validation button
      eValidate <- divClass "col" $ buttonClass "Validate" "btn"

      let
        prepare = ("data:application/json," <>) . Encoding.decodeUtf8 . LBS.toStrict . encode
      elDynAttr "a" (((("href" =:) . prepare) <$> d) <> pure ("download" =: "save.json" <> "class" =: "btn btn-secondary")) $
        text "Save"

      pure (current d <@ eValidate)

    -- the version where every change causes a validation
    -- pure (updated d)

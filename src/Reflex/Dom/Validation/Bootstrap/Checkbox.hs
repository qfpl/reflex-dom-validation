{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.Dom.Validation.Bootstrap.Checkbox (
    CheckboxOptionConfig(..)
  , CheckboxWidgetConfig(..)
  , checkboxWidget
  ) where

import Control.Monad (forM, forM_)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))

import Control.Lens

import Data.Text (Text)

import Data.Set (Set)
import qualified Data.Set as Set

import Reflex.Dom.Core

import Reflex.Dom.Validation

import Reflex.Dom.Validation.Bootstrap.Errors

data CheckboxOptionConfig c =
  CheckboxOptionConfig {
    _cocLabel :: Text
  , _cocId :: Text
  , _cocValue :: c
  }

makeLenses ''CheckboxOptionConfig

data CheckboxWidgetConfig c =
  CheckboxWidgetConfig {
    _cwcLabel :: Maybe Text
  , _cwcStacked :: Bool
  , _cwcValues :: [CheckboxOptionConfig c]
  }

makeLenses ''CheckboxWidgetConfig

checkboxWidget :: (MonadWidget t m, HasErrorMessage e, Ord c)
               => CheckboxWidgetConfig c
               -> ValidationWidget t m e (Wrap (Set c)) u
checkboxWidget cwc i dv _ des = divClass "form-group" $ do
  let
    it = idToText i
    cls = "form-check " <> bool " form-check-inline" "" (cwc ^. cwcStacked)
    wrapGroup = maybe id (const $ divClass "form-group") (cwc ^. cwcLabel)

  forM_ (cwc ^. cwcLabel) $
    elAttr "label" ("for" =: it) . text

  es <- wrapGroup . forM (cwc ^. cwcValues) $ \(CheckboxOptionConfig kl ki v) -> divClass cls $ do
    let
      itt = it <> ki
      f = maybe False (Set.member v) . unWrap
      dv' = f <$> dv
    iv <- sample . current $ dv'
    let ev = updated dv'

    cb <- checkbox iv $ def
      & setValue .~ ev
      & attributes .~ pure ("id" =: itt <> "type" =: "checkbox") <>
                    (("class" =:) . ("form-check-input " <>) <$> errorClass i des)
    elAttr "label" ("class" =: "form-check-label" <> "for" =: itt) $
      text kl
    let ev' = cb ^. checkbox_change

    errorsForId i des

    pure $ bool (Set.delete v) (Set.insert v) <$> ev'

  let
    eChange = (\fs -> Endo $ \(Wrap mss) -> Wrap . Just . fs $ fromMaybe mempty mss) <$> mergeWith (.) es

  pure $ ValidationWidgetOutput (pure mempty) eChange never

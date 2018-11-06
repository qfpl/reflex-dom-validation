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
module Reflex.Dom.Validation.Bootstrap.Radio (
    RadioOptionConfig(..)
  , RadioWidgetConfig(..)
  , radioWidget
  ) where

import Control.Monad (forM, forM_)
import Data.Bool (bool)
import Data.Monoid (Endo(..))

import Control.Lens

import Data.Text (Text)

import Reflex.Dom.Core

import Reflex.Dom.Validation

import Reflex.Dom.Validation.Bootstrap.Errors

data RadioOptionConfig c =
  RadioOptionConfig {
    _rocLabel :: Text
  , _rocId :: Text
  , _rocValue :: c
  }

makeLenses ''RadioOptionConfig

data RadioWidgetConfig c =
  RadioWidgetConfig {
    _rwcLabel :: Maybe Text
  , _rwcStacked :: Bool
  , _rwcValues :: [RadioOptionConfig c]
  }

makeLenses ''RadioWidgetConfig

radioWidget :: (MonadWidget t m, HasErrorMessage e, Eq c)
            => RadioWidgetConfig c
            -> ValidationWidget t m e (Wrap c)
radioWidget rwc i dv des = divClass "form-group" $ do
  forM_ (rwc ^. rwcLabel) $
    el "label" . text

  let
    it = idToText i
    cls = "form-check " <> bool " form-check-inline" "" (rwc ^. rwcStacked)

  es <- divClass "form-group" . forM (rwc ^. rwcValues) $ \(RadioOptionConfig kl ki v) -> divClass cls $ do
    let
      itt = it <> ki
      f = maybe False (== v) . unWrap
      dv' = f <$> dv
    iv <- sample . current $ dv'
    let ev = updated dv'

    cb <- checkbox iv $ def
      & setValue .~ ev
      & attributes .~ pure ("id" =: itt <> "type" =: "radio") <>
                    (("class" =:) . ("form-check-input " <>) <$> errorClass i des)
    elAttr "label" ("class" =: "form-check-label" <> "for" =: itt) $
      text kl
    let ev' = cb ^. checkbox_change

    errorsForId i des

    pure $ bool Nothing (Just v) <$> ev'

  pure $ Endo . const . Wrap <$> leftmost es

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
            -> ValidationWidget t m e (Wrap c) u
radioWidget rwc i dv _ des = divClass "form-group" $ do
  let
    it = idToText i
    cls = "form-check " <> bool " form-check-inline" "" (rwc ^. rwcStacked)
    wrapGroup = maybe id (const $ divClass "form-group") (rwc ^. rwcLabel)

  forM_ (rwc ^. rwcLabel) $
    elAttr "label" ("for" =: it) . text

  es <- wrapGroup . forM (rwc ^. rwcValues) $ \(RadioOptionConfig kl ki v) -> divClass cls $ do
    let
      itt = it <> ki
      f = (== Just v) . unWrap
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

  let
    eChange = Endo . const . Wrap <$> leftmost es

  pure $ ValidationWidgetOutput (pure mempty) eChange never

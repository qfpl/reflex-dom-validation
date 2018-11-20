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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Reflex.Dom.Validation.Bootstrap.Text (
    TextUpdate(..)
  , _UpdateOnChange
  , _UpdateOnEnter
  , TextWidgetConfig(..)
  , twcLabel
  , twcUpdate
  , textWidget
  , textAreaWidget
  ) where

import Control.Monad (join, guard, forM_)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Lens

import Reflex.Dom.Core

import GHCJS.DOM.Types (HTMLTextAreaElement)

import Reflex.Dom.Validation

import Reflex.Dom.Validation.Bootstrap.Errors

data TextUpdate =
    UpdateOnChange
  | UpdateOnEnter
  deriving (Eq, Ord, Show, Read)

makePrisms ''TextUpdate

data TextWidgetConfig =
  TextWidgetConfig {
    _twcLabel :: Maybe Text
  , _twcUpdate :: TextUpdate
  }

makeLenses ''TextWidgetConfig

textUpdate :: Reflex t
           => TextUpdate
           -> Dynamic t Text
           -> Event t Text
           -> Event t Word
           -> Event t Text
textUpdate UpdateOnChange _ e _ =
  e
textUpdate UpdateOnEnter d _ eK =
  current d <@ fmapMaybe (\n -> guard $ keyCodeLookup (fromIntegral n) == Enter) eK

textWidget :: (MonadWidget t m, HasErrorMessage e)
           => TextWidgetConfig
           -> ValidationWidget t m e (Wrap (Maybe Text)) u
textWidget twc i dv _ des = divClass "form-group" $ do
  let it = idToText i
  forM_ (twc ^. twcLabel) $
     elAttr "label" ("for" =: it) . text

  let
    f = fromMaybe "" . join . unWrap
    dv' = f <$> dv
  iv <- sample . current $ dv'
  let ev = updated dv'
  ti <- textInput $ def
    & textInputConfig_initialValue .~ iv
    & setValue .~ ev
    & attributes .~ pure ("id" =: it) <>
                    (("class" =:) . ("form-control " <>) <$> errorClass i des)
  let ev' = textUpdate (view twcUpdate twc) (value ti) (ti ^. textInput_input) (ti ^. textInput_keypress)

  errorsForId i des

  let
    eChange = Endo . const . Wrap . Just . (\t -> if Text.null t then Nothing else Just t) <$> ev'

  pure $ ValidationWidgetOutput (pure mempty) eChange never

textAreaWidget :: (MonadWidget t m, HasErrorMessage e)
               => TextWidgetConfig
               -> ValidationWidget t m e (Wrap (Maybe Text)) u
textAreaWidget twc i dv _ des = divClass "form-group" $ do
  let it = idToText i
  forM_ (twc ^. twcLabel) $
     elAttr "label" ("for" =: it) . text

  let
    f = fromMaybe "" . join . unWrap
    dv' = f <$> dv
  iv <- sample . current $ dv'
  let ev = updated dv'
  ta <- textArea $ def
    & textAreaConfig_initialValue .~ iv
    & setValue .~ ev
    & attributes .~ pure ("id" =: it) <>
                    (("class" =:) . ("form-control " <>) <$> errorClass i des)
  let ev' = textUpdate (view twcUpdate twc) (value ta) (ta ^. textArea_input) (ta ^. textArea_keypress)

  errorsForId i des

  let
    eChange = Endo . const . Wrap . Just . (\t -> if Text.null t then Nothing else Just t) <$> ev'

  pure $ ValidationWidgetOutput (pure mempty) eChange never

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
import Reflex.Dom.Validation.Error
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Requires
import Reflex.Dom.Validation.Wrap

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

class TextChange (r :: Requirement) where
  toText :: SRequirement r Text -> Wrap (Requires r Text) Maybe -> Text
  textChange :: Reflex t => SRequirement r Text -> Event t Text -> Event t (Endo (Wrap (Requires r Text) Maybe))

instance TextChange 'Required where
  toText (SRequired t) = fromMaybe t . unWrap
  textChange _ e = Endo . const . Wrap . (\t -> if Text.null t then Nothing else Just t) <$> e

instance TextChange 'Optional where
  toText _ = fromMaybe "" . join . unWrap
  textChange _ e = Endo . const . Wrap . Just . (\t -> if Text.null t then Nothing else Just t) <$> e

textWidget :: (MonadWidget t m, HasErrorMessage e, TextChange r)
           => TextWidgetConfig
           -> SRequirement r Text
           -> ValidationWidget t m e (Wrap (Requires r Text)) u
textWidget twc sr i dv _ des = divClass "form-group" $ do
  let it = idToText i
  forM_ (twc ^. twcLabel) $
     elAttr "label" ("for" =: it) . text

  let
    dv' = toText sr <$> dv
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
    eChange = textChange sr ev'

  pure $ ValidationWidgetOutput (pure mempty) eChange never

textAreaWidget :: (MonadWidget t m, HasErrorMessage e, TextChange r)
               => TextWidgetConfig
               -> SRequirement r Text
               -> ValidationWidget t m e (Wrap (Requires r Text)) u
textAreaWidget twc sr i dv _ des = divClass "form-group" $ do
  let it = idToText i
  forM_ (twc ^. twcLabel) $
     elAttr "label" ("for" =: it) . text

  let
    dv' = toText sr <$> dv
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
    eChange = textChange sr ev'

  pure $ ValidationWidgetOutput (pure mempty) eChange never

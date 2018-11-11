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
module Reflex.Dom.Validation.Bootstrap.Text (
    TextUpdate(..)
  , _UpdateOnChange
  , _UpdateOnEnter
  , TextWidgetConfig(..)
  , twcLabel
  , twcUpdate
  , textWidget
  ) where

import Control.Monad (join, forM_)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Lens

import Reflex.Dom.Core

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
           -> TextInput t
           -> Event t Text
textUpdate UpdateOnChange ti =
  ti ^. textInput_input
textUpdate UpdateOnEnter ti =
  current (value ti) <@ keypress Enter ti

textWidget :: (MonadWidget t m, HasErrorMessage e)
           => TextWidgetConfig
           -> ValidationWidget t m e (Wrap (Maybe Text))
textWidget twc i dv des = divClass "form-group" $ do
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
  let ev' = textUpdate (view twcUpdate twc) ti

  errorsForId i des

  pure . ValidationWidgetOutput (pure mempty) $
    Endo . const . Wrap . Just . (\t -> if Text.null t then Nothing else Just t) <$> ev'

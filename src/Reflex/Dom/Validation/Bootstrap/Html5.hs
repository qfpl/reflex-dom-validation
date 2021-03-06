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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Reflex.Dom.Validation.Bootstrap.Html5 where

import Control.Monad (forM_)
import Data.Monoid (Endo(..))

import Control.Lens

import Data.Text (Text)

import qualified Data.List.NonEmpty as NonEmpty

import Data.Validation
import Reflex.Dom.Core

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Error
import Reflex.Dom.Validation.Html5
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Requires
import Reflex.Dom.Validation.Wrap
import Reflex.Dom.Validation.Bootstrap.Errors

data ValidWidgetConfig t m e v a =
  ValidWidgetConfig {
    _vwcLabel :: Maybe Text
  , _vwcBuilder :: ValidInputConfigBuilder t m e v a
  }

makeLenses ''ValidWidgetConfig

validWidget ::
  ( MonadWidget t m
  , HasErrorMessage e
  , HasValidityError e
  )
  => ValidWidgetConfig t m e v a
  -> ValidationWidget t e (Wrap a) u v m ()
validWidget vwc = toValidationWidget_ $ \i dv du dc des -> divClass "form-group" $ do
  let it = idToText i

  forM_ (vwc ^. vwcLabel) $
     elAttr "label" ("for" =: it) . text

  vic <- runValidInputConfigBuilder (vwc ^. vwcBuilder) dv dc $
    pure ("id" =: it) <>
    (("class" =:) . ("form-control " <>) <$> errorClass i des)
  vi <- valid vic
  let
    (eFailure, eSuccess) = fanEither . fmap toEither . view viInput $ vi
  dFailure <- holdDyn [] . leftmost $
    [ fmap (WithId i) . NonEmpty.toList <$> eFailure
    , [] <$ eSuccess
    ]

  let
    eChange = Endo . const . Wrap <$> leftmost [Just <$> eSuccess, Nothing <$ eFailure]

  _ <- runValidationWidget errorsForId i dv du dc des

  pure $ ValidationWidgetOutput dFailure eChange never

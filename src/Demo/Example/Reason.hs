{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Demo.Example.Reason (
    AsReason(..)
  , HasReasonRequiredForIncomplete(..)
  , reasonF
  ) where

import Control.Monad (join)
import Data.Maybe (fromMaybe)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text


import qualified Data.List.NonEmpty as NE

import Data.Validation

import Reflex.Dom.Core

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Error
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Requires
import Reflex.Dom.Validation.Wrap

import Reflex.Dom.Validation.Bootstrap.Text

class HasReasonRequiredForIncomplete e where
  _ReasonRequiredForIncomplete :: Prism' e ()

class AsReason f where
  reason :: Lens' (f g) (Wrap (Maybe Text) g)

instance AsReason (Wrap (Maybe Text)) where
  reason = id

reasonV' :: HasReasonRequiredForIncomplete e
         => Id
         -> Wrap Bool Maybe
         -> Wrap (Maybe Text) Maybe
         -> Validation (NE.NonEmpty (WithId e)) (Wrap (Maybe Text) Identity)
reasonV' i (Wrap (Just False)) (Wrap (Just (Just t)))
  | Text.null t = Failure . pure . WithId i $ _ReasonRequiredForIncomplete # ()
  | otherwise  = Success . Wrap . Identity . Just $ t
reasonV' i (Wrap (Just False)) (Wrap _) =
  Failure . pure . WithId i $ _ReasonRequiredForIncomplete # ()
reasonV' _ _ (Wrap mmt) =
  Success . Wrap . Identity . Just . fromMaybe "" . join $ mmt

reasonW :: (MonadWidget t m, HasErrorMessage e)
        => ValidationWidget t e (Wrap (Maybe Text)) u v m ()
reasonW =
  textWidget (TextWidgetConfig (Just "Reason") UpdateOnChange) SOptional

reasonF :: (MonadWidget t m, HasErrorMessage e, HasReasonRequiredForIncomplete e, AsReason f)
        => Field t m e f (Wrap (Maybe Text)) u u (Wrap Bool Maybe) (Wrap Bool Maybe)
reasonF =
  Field reason id (flip const) (idApp "-r") (toValidationFn reasonV') reasonW

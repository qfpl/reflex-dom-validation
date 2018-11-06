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
  , reasonF
  ) where

import Control.Monad (join)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.Semigroup(Semigroup(..))

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Validation

import Reflex.Dom.Core

import Reflex.Dom.Validation

import Reflex.Dom.Validation.Bootstrap.Text

class AsReason f where
  reason :: Lens' (f g) (Wrap (Maybe Text) g)

instance AsReason (Wrap (Maybe Text)) where
  reason = id

reasonV :: ValidationFn e (Wrap (Maybe Text)) (Wrap (Maybe Text))
reasonV _ (Wrap (Just (Just t))) =
  Success . Wrap . Identity $
    if Text.null t then Nothing else Just t
reasonV _ _ =
  Success . Wrap . Identity $
    Nothing

reasonW :: (MonadWidget t m, HasErrorMessage e)
        => ValidationWidget t m e (Wrap (Maybe Text))
reasonW =
  textWidget (TextWidgetConfig (Just "Reason") UpdateOnChange)

reasonF :: (MonadWidget t m, HasErrorMessage e, AsReason f)
        => Field t m e f (Wrap (Maybe Text))
reasonF =
  Field reason (\i -> Id (Just i) "-r") reasonV reasonW

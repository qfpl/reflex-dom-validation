{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.Dom.Validation.Bootstrap.Workflow (
    workflowTemplate
  , workflowWidgetConfig
  , module Reflex.Dom.Validation.Workflow
  ) where

import Data.Bool (bool)
import Data.Monoid (Endo(..))

import Reflex.Dom.Core

import Bootstrap

import Reflex.Dom.Validation.Workflow

workflowTemplate :: MonadWidget t m
                 => Bool
                 -> Bool
                 -> m (Event t (Endo (f Maybe)))
                 -> m (Event t (), Event t (), Event t (Endo (f Maybe)))
workflowTemplate pageFirst pageLast w =
  divClass "container" $ do
    eChange <- divClass "row" . divClass "col" $ w
    (eBack, eNext) <- divClass "row" $ do
      eBack <- divClass "col" . buttonClass "Back" . ("btn" <>) $ bool "" " disabled" pageFirst
      eNext <- divClass "col" . buttonClass "Next" . ("btn" <>) $ bool "" " disabled" pageLast
      pure (eBack, eNext)
    pure (eBack, eNext, eChange)

workflowWidgetConfig :: MonadWidget t m
                     => WorkflowWidgetConfig t m f
workflowWidgetConfig =
  WorkflowWidgetConfig SaveStep ValidateStep workflowTemplate

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
module Reflex.Dom.Validation.Bootstrap.Workflow (
    workflowTemplate
  , workflowWidgetConfig
  , StepNavigation(..)
  , _ButtonNavigation
  , _DropdownNavigation
  , _ButtonAndDropdownNavigation
  , module Reflex.Dom.Validation.Workflow
  ) where

import Control.Monad (when)
import Data.Monoid (Endo(..))

import Control.Lens

import Control.Error

import Data.Text (Text)

import qualified Data.Map as Map

import Reflex.Dom.Core

import Bootstrap

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Workflow

data StepNavigation =
    ButtonNavigation
  | DropdownNavigation
  | ButtonAndDropdownNavigation
  deriving (Eq, Ord, Show, Read)

makePrisms ''StepNavigation

workflowHeader :: MonadWidget t m
               => StepNavigation
               -> Int
               -> [Text]
               -> m (Event t Int)
workflowHeader sn wix labels =
  case sn of
    ButtonNavigation -> do
      el "label" . text . fromMaybe "" $ atMay labels wix
      pure never
    _ -> do
      d <- bootstrapDropdown wix (pure . Map.fromList . zip [0..] $ labels) def
      pure $ d ^. dropdown_change

workflowFooter :: MonadWidget t m
               => StepNavigation
               -> Int
               -> Int
               -> m (Event t Int)
workflowFooter sn wix l =
  let
    isFirst = wix == 0
    isLast = wix == (l - 1)
  in
    if sn /= DropdownNavigation
    then do
      eBack' <- divClass "col" . buttonClass "Back" . ("btn" <>) $ bool "" " disabled" isFirst
      eNext' <- divClass "col" . buttonClass "Next" . ("btn" <>) $ bool "" " disabled" isLast
      let
        eBack = if isFirst then never else eBack'
        eNext = if isLast then never else eNext'
      pure . leftmost $ [(wix - 1) <$ eBack, (wix + 1) <$ eNext]
    else
      pure never

workflowTemplate :: MonadWidget t m
                 => StepNavigation
                 -> Int
                 -> Int
                 -> [Text]
                 -> m (ValidationWidgetOutput t e f)
                 -> m (Event t Int, ValidationWidgetOutput t e f)
workflowTemplate sn wix l labels w =
  divClass "container" $ do
    eIxDropdown <- divClass "row" $ workflowHeader sn wix labels
    eChange <- divClass "row" . divClass "col" $ w
    eIxButtons <- divClass "row" $ workflowFooter sn wix l
    let
      eIx = leftmost [eIxDropdown, eIxButtons]

    pure (eIx, eChange)

workflowWidgetConfig :: MonadWidget t m
                     => WorkflowWidgetConfig t m e f
workflowWidgetConfig =
  WorkflowWidgetConfig SaveStep ValidateStep (workflowTemplate ButtonAndDropdownNavigation)

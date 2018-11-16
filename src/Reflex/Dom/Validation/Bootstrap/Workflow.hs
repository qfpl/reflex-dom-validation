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

workflowFooterBack :: MonadWidget t m
                   => StepNavigation
                   -> Int
                   -> m (Event t Int)
workflowFooterBack sn wix =
  let
    isFirst = wix == 0
  in
    if sn /= DropdownNavigation
    then do
      eBack' <- divClass "col" . buttonClass "Back" . ("btn" <>) $ bool "" " disabled" isFirst
      let
        eBack = if isFirst then never else eBack'
      pure $ (wix - 1) <$ eBack
    else
      pure never

workflowFooterNext :: MonadWidget t m
                   => StepNavigation
                   -> Int
                   -> Int
                   -> m (Event t Int)
workflowFooterNext sn wix l =
  let
    isLast = wix == (l - 1)
  in
    if sn /= DropdownNavigation
    then do
      eNext' <- divClass "col" . buttonClass "Next" . ("btn" <>) $ bool "" " disabled" isLast
      let
        eNext = if isLast then never else eNext'
      pure $ (wix + 1) <$ eNext
    else
      pure never

workflowTemplate :: MonadWidget t m
                 => StepNavigation
                 -> Int
                 -> [Text]
                 -> m (ValidationWidgetOutput t e f)
                 -> m (Event t Int, ValidationWidgetOutput t e f)
workflowTemplate sn wix labels w =
  divClass "container" $ do
    eIxDropdown <- divClass "row" $ workflowHeader sn wix labels
    eChange <- divClass "row" . divClass "col" $ w
    eIxButtons <- divClass "row" $ do
      eBack <- workflowFooterBack sn wix
      eNext <- workflowFooterNext sn wix (length labels)
      pure . leftmost $ [eBack, eNext]
    let
      eIx = leftmost [eIxDropdown, eIxButtons]

    pure (eIx, eChange)

workflowWidgetConfig :: MonadWidget t m
                     => WorkflowWidgetConfig t m e f
workflowWidgetConfig =
  WorkflowWidgetConfig SaveStep ValidateStep (workflowTemplate ButtonAndDropdownNavigation)

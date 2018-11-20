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
    workflowWidgetConfig
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
               -> m a
               -> m (Event t Int)
workflowHeader sn wix labels m = divClass "col" $
  case sn of
    ButtonNavigation -> do
      el "label" . text . fromMaybe "" $ atMay labels wix
      _ <- m
      pure never
    _ -> do
      d <- bootstrapDropdown wix (pure . Map.fromList . zip [0..] $ labels) def
      _ <- m
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

workflowFooter :: MonadWidget t m
                   => StepNavigation
                   -> Int
                   -> [Text]
                   -> m a
                   -> m (Event t Int)
workflowFooter sn wix labels m = do
  eBack <- workflowFooterBack sn wix
  _ <- m
  eNext <- workflowFooterNext sn wix (length labels)
  pure . leftmost $ [eBack, eNext]

workflowCombine :: MonadWidget t m
                => m (Event t Int)
                -> m (ValidationWidgetOutput t e f u)
                -> m (Event t Int)
                -> m (Event t Int, ValidationWidgetOutput t e f u)
workflowCombine h m f =
  divClass "container" $ do
    eH <- divClass "row" h
    e <- divClass "row" . divClass "col" $ m
    eF <- divClass "row" f
    pure (leftmost [eH, eF], e)

workflowWidgetConfig :: MonadWidget t m
                     => WorkflowWidgetConfig t m e
workflowWidgetConfig =
  WorkflowWidgetConfig
    SaveStep
    ValidateStep
    (workflowHeader ButtonAndDropdownNavigation)
    (workflowFooter ButtonAndDropdownNavigation)
    workflowCombine

{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.Dom.Validation.Workflow (
    PageRequirement(..)
  , _NoRequirement
  , _SaveFirst
  , _ValidateFirst
  , WorkflowStep(..)
  , WorkflowWidgetConfig(..)
  , wwcBackRequirement
  , wwcNextRequirement
  , wwcTemplate
  , workflowWidget
  ) where

import Control.Monad (void)
import Data.Monoid (Endo(..))

import Control.Lens

import Reflex.Dom.Core
import Data.Validation (toEither)

import Reflex.Dom.Validation

data PageRequirement =
    NoRequirement
  | SaveFirst
  | ValidateFirst
  deriving (Eq, Ord, Show, Read)

makePrisms ''PageRequirement

data WorkflowStep t m e f where
  WorkflowStep :: Field t m e f f' -> WorkflowStep t m e f

data WorkflowWidgetConfig t m f =
  WorkflowWidgetConfig {
    _wwcBackRequirement :: PageRequirement
  , _wwcNextRequirement :: PageRequirement
  , _wwcTemplate :: Bool -> Bool -> m (Event t (Endo (f Maybe))) -> m (Event t (), Event t (), Event t (Endo (f Maybe)))
  }

makeLenses ''WorkflowWidgetConfig

workflowWidget :: MonadWidget t m
               => WorkflowWidgetConfig t m f
               -> [WorkflowStep t m e f]
               -> ValidationWidget t m e f
workflowWidget _ [] _ _ _ =
  pure never
workflowWidget wwc steps i dv des =
  let
    l = length steps
    w wix =
      let
        step = steps !! wix
      in case step of
        WorkflowStep f -> Workflow $ do
          (eBack, eNext, eChange) <- (wwc ^. wwcTemplate) (wix == 0) (wix == (l - 1)) $
            fieldWidget f i dv des

          iv' <- sample . current $ dv
          dv' <- foldDyn ($) iv' $ fmap appEndo eChange

          let
            eBackChange = case wwc ^. wwcBackRequirement of
              NoRequirement -> mempty <$ eBack
              _ -> never
            eNextChange = case wwc ^. wwcNextRequirement of
              NoRequirement -> mempty <$ eNext
              _ -> never
            checkValidation e =
              let
                (_, eSuccess) = fanEither $ toEither . fieldValidation f i <$> current dv' <@ e
              in
                void eSuccess
            eBackW = case wwc ^. wwcBackRequirement of
              ValidateFirst -> checkValidation eBack
              _ -> eBack
            eNextW = case wwc ^. wwcNextRequirement of
              ValidateFirst -> checkValidation eNext
              _ -> eNext

            eChange' = leftmost [eBackChange, eNextChange, eChange]
            eW = leftmost [w (wix - 1) <$ eBackW, w (wix + 1) <$ eNextW]

          pure (eChange', eW)
  in do
    de <- workflow $ w 0
    pure $ switchDyn de

{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Functor.Identity (Identity(..))
import Data.Monoid (Endo(..))

import Control.Lens

import qualified Data.List.NonEmpty as NE

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

workflowWidget :: forall t m e f.
                  (MonadWidget t m, NFunctor f)
               => [WorkflowStep t m e f]
               -> WorkflowWidgetConfig t m f
               -> ValidationWidget t m e f
workflowWidget [] _ _ _ _ =
  pure never
workflowWidget steps wwc i dv des =
  let
    l = length steps
    w wix iv =
      let
        step = steps !! wix
      in case step of
        WorkflowStep f@(Field fl _ _ _) -> Workflow $ mdo
          let
            isFirst = wix == 0
            isLast = wix == (l - 1)
          (eBack', eNext', eChange) <- (wwc ^. wwcTemplate) isFirst isLast $
            fieldWidget f i dv $ (++) <$> des <*> dFailure
          let
            eBack = if isFirst then never else eBack'
            eNext = if isLast then never else eNext'

          -- TODO bring updated dv changes in here?
          dv' <- foldDyn ($) iv $ fmap appEndo eChange

          let
            eBackChange = case wwc ^. wwcBackRequirement of
              NoRequirement -> mempty <$ eBack
              _ -> never
            eNextChange = case wwc ^. wwcNextRequirement of
              NoRequirement -> mempty <$ eNext
              _ -> never

            checkValidation e =
              let
                (eF, eS) = fanEither $ toEither . fieldValidation f i <$> current dv' <@ e
              in
                (eF, flip (set fl) <$> current dv' <@> (nmap (Just . runIdentity) <$> eS))

            (eBackE, eBackW) = case wwc ^. wwcBackRequirement of
              ValidateFirst -> checkValidation eBack
              _ -> (never, current dv' <@ eBack)
            (eNextE, eNextW) = case wwc ^. wwcNextRequirement of
              ValidateFirst -> checkValidation eNext
              _ -> (never, current dv' <@ eNext)
            eFailure = eBackE <> eNextE

            eChange' = leftmost [eBackChange, eNextChange, eChange]
            eW = leftmost [w (wix - 1) <$> eBackW, w (wix + 1) <$> eNextW]

          dFailure <- holdDyn mempty . leftmost $ [NE.toList <$> eFailure, [] <$ eW]

          pure (eChange', eW)
  in do
    iv <- sample . current $ dv
    de <- workflow $ w 0 iv
    pure $ switchDyn de

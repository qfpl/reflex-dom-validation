{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Dom.Validation.Workflow (
    StepRequirement(..)
  , _BlankStep
  , _SaveStep
  , _ValidateStep
  , StepNavigation(..)
  , _ButtonNavigation
  , _ButtonNavigationWithStepLabels
  , _DropdownNavigation
  , _ButtonAndDropdownNavigation
  , WorkflowStep(..)
  , WorkflowWidgetConfig(..)
  , wwcBackRequirement
  , wwcNextRequirement
  , wwcNavigation
  , wwcTemplate
  , workflowWidget
  ) where

import Control.Monad (join)
import Data.Functor.Identity (Identity(..))
import Data.List (nub)
import Data.Monoid (Endo(..))

import Control.Lens
import Control.Error

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.List.NonEmpty as NE

import Reflex.Dom.Core
import Data.Validation (toEither)

import Reflex.Dom.Validation

data StepRequirement =
    BlankStep
  | SaveStep
  | ValidateStep
  deriving (Eq, Ord, Show, Read)

makePrisms ''StepRequirement

data WorkflowStep t m e f where
  WorkflowStep :: Text -> Field t m e f f' -> WorkflowStep t m e f

stepLabel :: WorkflowStep t m e f -> Text
stepLabel (WorkflowStep l _) = l

data StepNavigation =
    ButtonNavigation
  | ButtonNavigationWithStepLabels
  | DropdownNavigation
  | ButtonAndDropdownNavigation
  deriving (Eq, Ord, Show, Read)

makePrisms ''StepNavigation

data WorkflowWidgetConfig t m e f =
  WorkflowWidgetConfig {
    _wwcBackRequirement :: StepRequirement
  , _wwcNextRequirement :: StepRequirement
  , _wwcNavigation :: StepNavigation
  , _wwcTemplate :: StepNavigation
                 -> Int
                 -> Int
                 -> [Text]
                 -> m (ValidationWidgetOutput t e f)
                 -> m (Event t Int, ValidationWidgetOutput t e f)
  }

makeLenses ''WorkflowWidgetConfig

workflowWidget :: forall t m e f.
                  (MonadWidget t m, Eq e, NFunctor f)
               => [WorkflowStep t m e f]
               -> WorkflowWidgetConfig t m e f
               -> ValidationWidget t m e f
workflowWidget [] _ _ _ _ =
  pure mempty
workflowWidget steps wwc i dv des =
  let
    labels = fmap stepLabel steps
    l = length steps
    w wix iv =
      let
      in case atMay steps wix of
        Nothing -> Workflow $ do
          text "Indexing error"
          pure (mempty, never)
        Just (WorkflowStep _ f@(Field fl _ _ _)) -> Workflow $ mdo
          (eIx, ValidationWidgetOutput dFailure eChange) <- (wwc ^. wwcTemplate) (wwc ^. wwcNavigation) wix l labels $
            fieldWidget f i dv des
          let
            eBack = ffilter (< wix) eIx
            eNext = ffilter (> wix) eIx

          dv' <- foldDyn ($) iv  . mergeWith (.) $ [
              fmap appEndo eChange
              -- TODO double check this
            , flip const <$> updated dv
            ]

          let
            eBackChange = case wwc ^. wwcBackRequirement of
              BlankStep -> Endo (const iv) <$ eBack
              _ -> never
            eNextChange = case wwc ^. wwcNextRequirement of
              BlankStep -> Endo (const iv) <$ eNext
              _ -> never
            eChange' = leftmost [eBackChange, eNextChange, eChange]

            checkValidation e =
              let
                (eF, eIS) = fanEither $ (\v ix -> fmap (\s -> (ix, s)) . toEither . fieldValidation f i $ v) <$> current dv' <@> e
              in
                (eF, (\v (i, s) -> (i, set fl (nmap (Just . runIdentity) s) v)) <$> current dv' <@> eIS)

            (eBackE, eBackW) = case wwc ^. wwcBackRequirement of
              ValidateStep -> checkValidation eBack
              _ -> (never, (\x y -> (y, x)) <$> current dv' <@> eBack)
            (eNextE, eNextW) = case wwc ^. wwcNextRequirement of
              ValidateStep -> checkValidation eNext
              _ -> (never, (\x y -> (y, x)) <$> current dv' <@> eNext)
            eFailure = eBackE <> eNextE

            eW = uncurry w <$> leftmost [eBackW, eNextW]

          iFailure <- sample . current $ dFailure
          dFailure' <- holdDyn iFailure . leftmost $
            [ mergeWith (<>) [updated dFailure, NE.toList <$> eFailure]
            , [] <$ eW
            ]

          pure (ValidationWidgetOutput dFailure' eChange', eW)
  in do
    iv <- sample . current $ dv
    dvwo <- workflow $ w 0 iv
    let
      dd = _vwoFailures <$> dvwo
      de = _vwoSuccesses <$> dvwo
    pure $ ValidationWidgetOutput (join dd) (switchDyn de)

{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Validation.Workflow (
    FieldCheck(..)
  , FieldTransitionAction(..)
  , FieldAction(..)
  , WorkflowForm(..)
  , WorkflowPiece(..)
  , nestWorkflow
  , WLDirection(..)
  , workflowList
  , workflowField
  , workflowWidget
  ) where

import Control.Monad (void)

import Data.Monoid (Endo(..))

import Control.Monad.Trans (lift)

import Control.Lens

import Reflex.Dom.Core

import Reflex.Dom.Validation

newtype WorkflowForm t m a =
  WorkflowForm {
    unWorkflowForm :: a -> Workflow t (EventWriterT t (Endo a) m) ()
  }

data FieldCheck =
  Validated | Unvalidated
  deriving (Eq, Ord, Show, Read)

data FieldTransitionAction =
  Update | OldValue | EmptyValue
  deriving (Eq, Ord, Show, Read)

data FieldAction t m a =
  FieldAction {
    faWorkflow :: WorkflowForm t m a
  , faCheck :: FieldCheck
  , faTransition :: FieldTransitionAction
  }

splitCheck :: FieldAction t m a
           -> Either (FieldAction t m a) (FieldAction t m a)
splitCheck fa@(FieldAction _ Validated _) = Left fa
splitCheck fa@(FieldAction _ Unvalidated _) = Right fa

toWorkflow :: NFunctor a'
           => Lens' (a Maybe) (a' Maybe)
           -> a Maybe
           -> a' Maybe
           -> a Maybe
           -> FieldAction t m (a Maybe)
           -> Workflow t (EventWriterT t (Endo (a Maybe)) m) ()
toWorkflow l _ na' a (FieldAction fw _ Update) =
  unWorkflowForm fw (set l na' a)
toWorkflow _ oa' _ _ (FieldAction fw _ OldValue) =
  unWorkflowForm fw oa'
toWorkflow l _ _ a (FieldAction fw _ EmptyValue) =
  unWorkflowForm fw (set l nempty a)

actionToEndo :: NFunctor a'
             => Lens' (a Maybe) (a' Maybe)
             -> a Maybe
             -> FieldAction t m (a Maybe)
             -> Maybe (Endo (a Maybe))
actionToEndo _ _ (FieldAction _ _ Update) =
  Nothing
actionToEndo _ o (FieldAction _ _ OldValue) =
  Just . Endo . const $ o
actionToEndo l _ (FieldAction _ _ EmptyValue) =
  Just . Endo $ set l nempty

type WorkflowPiece t m r e a =
  Piece t m r e a (Event t (FieldAction t m (a Maybe)))

workflowField :: (Reflex t, Monad m) => Field t m r e a -> WorkflowPiece t m r e a
workflowField =
  FieldPiece (pure never) (pure never)

workflowWidget :: MonadWidget t m => m x -> WorkflowPiece t m r e a
workflowWidget m =
  WidgetPiece (m >> pure never)

nestWorkflow :: forall t m r e a. (MonadWidget t m, NFunctor a)
             => ((WorkflowPiece t m r e a -> WorkflowForm t m (a Maybe)) -> WorkflowForm t m (a Maybe))
             -> FieldWidget t m r e a
nestWorkflow mkW = FieldWidget' $ \dr i a ea ev -> mdo
  let
    ea' = leftmost [ea, updated dState]

    w = mkW $ \wp -> case wp of
      FieldPiece pre post (Field ctx idFn l fw) -> WorkflowForm $ \a' -> Workflow $ mdo
        ePre <- lift pre
        (dc, ec) <- lift $
          runFieldWidget
            fw
            (ctx <$> dr <*> dState)
            (idFn i)
            (view l a')
            (view l <$> ea')
            (leftmost [ev, void eChangeV])
        ePost <- lift post
        let
          eChange = leftmost [ePre, ePost]
          (eChangeV, eChangeU) = fanEither $ splitCheck <$> eChange
          eChange' = leftmost [coincidence $ eChangeV <$ ec, eChangeU]

        tellEvent $ Endo . set l <$> updated dc
        tellEvent $ fmapMaybe (actionToEndo l a') eChange'

        pure ((), toWorkflow l a' <$> current dc <*> current dState <@> eChange')
      WidgetPiece p -> WorkflowForm $ \a' -> Workflow $ do
        eChange <- lift p
        pure ((), ($ a') . unWorkflowForm . faWorkflow <$> eChange)

  (_, eE) <- runEventWriterT $ workflow $ unWorkflowForm w a
  dState <- foldDyn ($) a $ appEndo <$> eE
  pure (dState, fmapMaybe id $ ntraverse (fmap Identity) <$> updated dState)

data WLDirection =
  WLBack | WLNext
  deriving (Eq, Ord, Show, Read)

workflowList :: (MonadWidget t m, NFunctor a)
             => (Bool -> Bool -> m (Event t WLDirection))
             -> [WorkflowPiece t m r e a]
             -> FieldWidget t m r e a
workflowList _ [] =
  blankFieldWidget
workflowList fn ps = nestWorkflow $ \wfn ->
  let
    len = length ps

    toAction i WLBack =
      FieldAction (ps' !! (i - 1)) Unvalidated Update
    toAction i WLNext =
      FieldAction (ps' !! (i + 1)) Validated Update

    mkButton i = do
      e <- fn (i == 0) (i == len - 1)
      pure $ toAction i <$> e

    mkPost post i = do
      e1 <- post
      e2 <- mkButton i
      pure . leftmost $ [e1, e2]
    f i (FieldPiece pre post f) =
      wfn $ FieldPiece pre (mkPost post i) f
    f i (WidgetPiece m) =
      wfn $ WidgetPiece (m >> mkButton i)

    ps' = zipWith f [0..] ps
  in
    head ps'

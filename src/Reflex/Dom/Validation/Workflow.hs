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
  , WorkflowPiece
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

import Data.Validation (toEither)

data FieldCheck =
  Validated | Unvalidated
  deriving (Eq, Ord, Show, Read)

data FieldTransitionAction =
  Update | OldValue | EmptyValue
  deriving (Eq, Ord, Show, Read)

data FieldAction t m e a =
  FieldAction {
    faWorkflow :: Workflow t (EventWriterT t (NestW e a) m) ()
  , faCheck :: FieldCheck
  , faTransition :: FieldTransitionAction
  }

splitCheck :: FieldAction t m e a
           -> Either (FieldAction t m e a) (FieldAction t m e a)
splitCheck fa@(FieldAction _ Validated _) = Left fa
splitCheck fa@(FieldAction _ Unvalidated _) = Right fa

actionToEndo :: NFunctor a'
             => Lens' (a Maybe) (a' Maybe)
             -> a Maybe
             -> FieldAction t m e a
             -> Maybe (Endo (a Maybe))
actionToEndo _ _ (FieldAction _ _ Update) =
  Nothing
actionToEndo _ o (FieldAction _ _ OldValue) =
  Just . Endo . const $ o
actionToEndo l _ (FieldAction _ _ EmptyValue) =
  Just . Endo $ set l nempty

type WorkflowPiece t m r e a =
  Piece t m r e a (Event t (FieldAction t m e a))

workflowField :: (Reflex t, Monad m) => Field t m r e a -> WorkflowPiece t m r e a
workflowField =
  FieldPiece (pure never) (pure never)

workflowWidget :: MonadWidget t m => m x -> WorkflowPiece t m r e a
workflowWidget m =
  WidgetPiece (m >> pure never)

nestWorkflow :: forall t m r e a. (MonadWidget t m, NFunctor a)
             => ((F t m r e a -> Event t (FieldAction t m e a)-> EventWriterT t (NestW e a) m (Event t (FieldAction t m e a))) -> Workflow t (EventWriterT t (NestW e a) m) ())
             -> FW t m r e a
nestWorkflow mkW = FW . FW' $ \i dr da db ev -> do
  let
    w =
      mkW $ \(F iFn rFn l (FW (FW' fn))) e -> do
        ia <- sample . current $ da
        da' <- holdUniqDyn $ view l <$> da
        db' <- holdUniqDyn $ view l <$> db
        let (eChecked, eUnchecked) = fanEither $ splitCheck <$> e
        (ea, eb) <- lift $
          fn
            (iFn i)
            (rFn <$> dr <*> da)
            da'
            db'
            (leftmost [ev, void eChecked])
        tellEvent $ toPre . Endo . over l . appEndo <$> ea
        tellEvent $ toPost . Endo . over l . appEndo <$> eb


        let
          eb' = (\x (Endo f) -> f x) <$> current db' <@> eb
          eb'' = fmapMaybe id $ ntraverse (either (const Nothing) (Just . Identity) . toEither) <$> eb'
          e' = leftmost [eUnchecked, coincidence $ eChecked <$ eb'']
        tellEvent . fmapMaybe id $ fmap toPre . actionToEndo l ia <$> e'

        pure e'

  (_, eE) <- runEventWriterT $ workflow $ w
  pure (_nwPreValidation <$> eE, _nwPostValidation <$> eE)

data WLDirection =
  WLBack | WLNext
  deriving (Eq, Ord, Show, Read)

workflowList :: (MonadWidget t m, NFunctor a)
             => (Bool -> Bool -> m (Event t WLDirection))
             -> [F t m r e a]
             -> FW t m r e a
workflowList _ [] =
  blankFW
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

    f i field = Workflow $ mdo
      eButton' <- wfn field eButton
      eButton <- lift $ mkButton i
      pure ((), faWorkflow <$> eButton')

    ps' = zipWith f [0..] ps
  in
    head ps'

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
  , WorkflowStep(..)
  , WorkflowWidgetConfig(..)
  , wwcBackRequirement
  , wwcNextRequirement
  , wwcTemplate
  , workflowWidget
  , HasBadWorkflowIndex(..)
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
import Data.Validation

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

data WorkflowWidgetConfig t m e f =
  WorkflowWidgetConfig {
    _wwcBackRequirement :: StepRequirement
  , _wwcNextRequirement :: StepRequirement
  , _wwcTemplate :: Int
                 -> [Text]
                 -> m (ValidationWidgetOutput t e f)
                 -> m (Event t Int, ValidationWidgetOutput t e f)
  }

makeLenses ''WorkflowWidgetConfig

foldValidation :: [e] -> Either (NE.NonEmpty e) x -> Either (NE.NonEmpty e) x
foldValidation es (Left (x NE.:| xs)) = Left (x NE.:| (xs ++ es))
foldValidation [] (Right x) = Right x
foldValidation (e:es) _ = Left (e NE.:| es)

class HasBadWorkflowIndex e where
  _BadWorkflowIndex :: Prism' e Int

-- validateBetween :: (NFunctor f, HasBadWorkflowIndex e)
--                 => [WorkflowStep t m e f]
--                 -> Id
--                 -> Int
--                 -> [WithId e]
--                 -> f Maybe
--                 -> Int
--                 -> (Int, Either (NE.NonEmpty (WithId e)) (f Maybe))
-- validateBetween steps i ixStart (e:es) v ixStop =
--   (ixStart, Left $ e NE.:| es)
-- validateBetween steps i ixStart [] v ixStop =
--   case atMay steps ixStart of
--     Nothing ->
--       (ixStart, Left $ (WithId i $ _BadWorkflowIndex # ixStart) NE.:| [])
--     Just (WorkflowStep _ f@(Field fl _ _ _)) ->
--       case compare ixStart ixStop of
--           EQ -> (ixStart, Right v)
--           LT -> case fmap (flip (set fl) v . nmap (Just . runIdentity)) . toEither . fieldValidation f i $ v of
--             Left e -> (ixStart, Left e)
--             Right v' -> validateBetween steps i (ixStart + 1) [] v' ixStop
--           GT -> case fmap (flip (set fl) v . nmap (Just . runIdentity)) . toEither . fieldValidation f i $ v of
--             Left e -> (ixStart, Left e)
--             Right v' -> validateBetween steps i (ixStart - 1) [] v' ixStop

workflowWidget :: forall t m e f.
                  (MonadWidget t m, Eq e, HasBadWorkflowIndex e, NFunctor f)
               => [WorkflowStep t m e f]
               -> WorkflowWidgetConfig t m e f
               -> ValidationWidget t m e f
workflowWidget [] _ _ _ _ =
  pure mempty
workflowWidget steps wwc i dv des =
  let
    labels = fmap stepLabel steps
    w wix iv =
      let
      in case atMay steps wix of
        Nothing -> Workflow $ do
          text "Indexing error"
          pure (mempty, never)
        Just (WorkflowStep _ f@(Field fl _ _ _)) -> Workflow $ mdo
          (eIx, ValidationWidgetOutput dFailure eChange) <- (wwc ^. wwcTemplate) wix labels $
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
                fes es v ix = foldValidation es . fmap (\s -> (ix, s)) . toEither . fieldValidation f i $ v
                (eF, eIS) = fanEither $ fes <$> current dFailure <*> current dv' <@> e
                -- TODO validate from here to the new index, stop at first validation error
                -- (eF, eIS) = fanEither $ validateBetween steps i wix <$> current dFailure <*> current dv' <@> e
              in
                (eF, (\v (i, s) -> (i, set fl (nmap (Just . runIdentity) s) v)) <$> current dv' <@> eIS)

            -- checkValidation e =
            --   let
            --     eEIS = validateBetween steps i wix <$> current dFailure <*> current dv' <@> e
            --     fes v (i, Left e) = (Just e, (i, v))
            --     fes _ (i, Right s) = (Nothing, (i, s))
            --     eMEIS = fes <$> current dv' <@> eEIS
            --     eME = fmapMaybe fst eMEIS
            --     eIS = fmap snd eMEIS
            --   in
            --     (eME, eIS)

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

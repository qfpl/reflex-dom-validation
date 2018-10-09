{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Demo.Example.CompletedWithReason (
    HasReasonRequiredForIncomplete(..)
  , AsCompletedWithReason(..)
  , CompletedWithReason(..)
  , completedWithReasonF
  ) where

import Data.Semigroup(Semigroup(..))
import Data.Proxy(Proxy(..))

import Control.Lens

import Data.Text (Text)

import Data.Validation

import Reflex.Dom.Core

import Reflex.Dom.Validation

import Demo.Example.Completed
import Demo.Example.Reason

class HasReasonRequiredForIncomplete e where
  _ReasonRequiredForIncomplete :: Prism' e ()

data CompletedWithReason f = CompletedWithReason {
    _cwrCompleted :: Wrap Bool f
  , _cwrReason :: Wrap (Maybe Text) f
  }

deriving instance (Eq (f Bool), Eq (f (Maybe Text))) => Eq (CompletedWithReason f)
deriving instance (Ord (f Bool), Ord (f (Maybe Text))) => Ord (CompletedWithReason f)
deriving instance (Show (f Bool), Show (f (Maybe Text))) => Show (CompletedWithReason f)
deriving instance (Read (f Bool), Read (f (Maybe Text))) => Read (CompletedWithReason f)

makeLenses ''CompletedWithReason

instance AsCompleted CompletedWithReason where
  completed = cwrCompleted

instance AsReason CompletedWithReason where
  reason = cwrReason

completedWithReasonV :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, HasReasonRequiredForIncomplete e)
                     => Proxy t -> Proxy m -> ValidationFn e CompletedWithReason CompletedWithReason
completedWithReasonV _ _ i cr =
  let
    fC = completedF :: Field t m e CompletedWithReason (Wrap Bool)
    vC = fieldValidation fC i cr
    fR = reasonF :: Field t m e CompletedWithReason (Wrap (Maybe Text))
    vR = fieldValidation fR i cr
    f c r =
      if unwrapV c == False && unwrapV r == Nothing
      then Failure . pure . WithId (fieldId fR i) $ _ReasonRequiredForIncomplete # ()
      else Success $ CompletedWithReason c r
  in
    vC `bindValidation` \c ->
    vR `bindValidation` \r ->
    f c r

completedWithReasonW :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e)
                     => ValidationWidget t m e CompletedWithReason
completedWithReasonW i dv de = do
  eC <- fieldWidget completedF i dv de
  eR <- fieldWidget reasonF i dv de
  pure $ eC <> eR

  -- let
  --   wComplete = Workflow $ do
  --     e <- fieldWidget completedF i dv de
  --     eNext <- buttonClass "Next" "btn"
  --     pure (e, wReason <$ eNext)
  --   wReason = Workflow $ do
  --     e <- fieldWidget reasonF i dv de
  --     eBack <- buttonClass "Back" "btn"
  --     pure (e, wComplete <$ eBack)

  -- de <- workflow wComplete
  -- pure . switchDyn $ de

class AsCompletedWithReason f where
  completedWithReason :: Lens' (f g) (CompletedWithReason g)

instance AsCompletedWithReason CompletedWithReason where
  completedWithReason = id

completedWithReasonF :: forall t m e f. (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, HasReasonRequiredForIncomplete e, AsCompletedWithReason f)
                     => Field t m e f CompletedWithReason
completedWithReasonF =
  Field completedWithReason (\i -> Id (Just i) "-cwr") (completedWithReasonV (Proxy :: Proxy t) (Proxy :: Proxy m)) completedWithReasonW

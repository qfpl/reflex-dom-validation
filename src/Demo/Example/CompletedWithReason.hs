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
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Demo.Example.CompletedWithReason (
    HasReasonRequiredForIncomplete(..)
  , AsCompletedWithReason(..)
  , CompletedWithReason(..)
  , completedWithReasonF
  ) where

import GHC.Generics (Generic)

import Data.Semigroup(Semigroup(..))
import Data.Proxy(Proxy(..))

import Control.Lens

import Data.Aeson (ToJSON, FromJSON, ToJSON1, FromJSON1)

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
  } deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup1 f => Semigroup (CompletedWithReason f) where
  CompletedWithReason c1 r1 <> CompletedWithReason c2 r2 = CompletedWithReason (c1 <> c2) (r1 <> r2)

instance Monoid1 f => Monoid (CompletedWithReason f) where
  mempty = CompletedWithReason mempty mempty
  mappend = (<>)

instance ToJSON1 f => ToJSON (CompletedWithReason f) where
instance FromJSON1 f => FromJSON (CompletedWithReason f) where

instance NFunctor CompletedWithReason where
  nmap f (CompletedWithReason c r) = CompletedWithReason (nmap f c) (nmap f r)

makeLenses ''CompletedWithReason

instance AsCompleted CompletedWithReason where
  completed = cwrCompleted

instance AsReason CompletedWithReason where
  reason = cwrReason

completedWithReasonV :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, HasReasonRequiredForIncomplete e)
                     => Proxy t -> Proxy m -> ValidationFn e CompletedWithReason CompletedWithReason
completedWithReasonV _ _ i cr =
  let
    fC = completedF :: Field t m e CompletedWithReason (Wrap Bool) () ()
    fR = reasonF :: Field t m e CompletedWithReason (Wrap (Maybe Text)) () ()
    f c r =
      if unwrapV c == False && unwrapV r == Nothing
      then Failure . pure . WithId (fieldId fR i) $ _ReasonRequiredForIncomplete # ()
      else Success $ CompletedWithReason c r
  in
    fieldValidation fC i cr `bindValidation` \c ->
    fieldValidation fR i cr `bindValidation` \r ->
    f c r

completedWithReasonW :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e)
                     => ValidationWidget t m e CompletedWithReason u
completedWithReasonW i dv du  de = do
  eC <- fieldWidget completedF i dv du de
  eR <- fieldWidget reasonF i dv du de
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

completedWithReasonF :: forall t m e f u. (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, HasReasonRequiredForIncomplete e, AsCompletedWithReason f)
                     => Field t m e f CompletedWithReason u ()
completedWithReasonF =
  Field completedWithReason united (idApp "-cwr") (completedWithReasonV (Proxy :: Proxy t) (Proxy :: Proxy m)) completedWithReasonW

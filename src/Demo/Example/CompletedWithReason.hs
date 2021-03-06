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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Demo.Example.CompletedWithReason (
    AsCompletedWithReason(..)
  , CompletedWithReason(..)
  , completedWithReasonF
  ) where

import GHC.Generics (Generic)

import Data.Semigroup(Semigroup(..))

import Control.Lens

import Data.Aeson (ToJSON, FromJSON, ToJSON1, FromJSON1)

import Data.Text (Text)

import Data.Validation

import Reflex.Dom.Core

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Classes
import Reflex.Dom.Validation.Error
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Wrap

import Demo.Example.Completed
import Demo.Example.Reason

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

class AsCompletedWithReason f where
  completedWithReason :: Lens' (f g) (CompletedWithReason g)

instance AsCompletedWithReason CompletedWithReason where
  completedWithReason = id

completedWithReasonF :: forall t m e f u v.
                     ( MonadWidget t m
                     , HasErrorMessage e
                     , HasNotSpecified e
                     , HasReasonRequiredForIncomplete e
                     , AsCompletedWithReason f
                     )
                     => Field t m e f CompletedWithReason u u v (Wrap Bool Maybe)
completedWithReasonF =
  let
    -- fC =
    --   completedF :: Field t m e CompletedWithReason (Wrap Bool) u u v va
    -- fR =
    --   reasonF :: Field t m e CompletedWithReason (Wrap (Maybe Text)) u u (Wrap Bool Maybe) (Wrap Bool Maybe)

    -- f i c r =
    --   if unwrapV c == False && unwrapV r == Nothing
    --   then Failure . pure . WithId (fieldId fR i) $ _ReasonRequiredForIncomplete # ()
    --   else Success $ CompletedWithReason c r

    -- completedWithReasonV = toValidationFn $ \i v cr ->
    --   runValidationFn (fieldValidation fC) i v cr `bindValidation` \c ->
    --   runValidationFn (fieldValidation fR) i v cr `bindValidation` \r ->
    --   f i c r
    completedWithReasonV =
      CompletedWithReason <$> fieldValidation (completedF @t @m) <*> fieldValidation (reasonF @t @m)

    completedWithReasonW =
      fieldWidget completedF >>
      fieldWidget reasonF
  in
    Field completedWithReason id (const . view (completedWithReason . completed)) (idApp "-cwr") completedWithReasonV completedWithReasonW

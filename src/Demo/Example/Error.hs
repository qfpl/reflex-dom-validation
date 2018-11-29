{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Demo.Example.Error (
    MyError(..)
  ) where

import GHC.Generics (Generic)

import Data.Aeson (ToJSON, FromJSON)

import Control.Lens.TH

import qualified Data.Text as Text

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Error
import Reflex.Dom.Validation.Html5
import Reflex.Dom.Validation.Workflow

import Demo.Example.CompletedWithReason
import Demo.Example.TestCollections
import Demo.Example.Workflow

data MyError =
    MENotSpecified
  | MEReasonRequiredForIncomplete
  | MECollectionTooSmall
  | MEFooNotDigits
  | MEFooNotLower
  | MEFooNotUpper
  | MEValidity ValidityError
  | MEBadWorkflowIndex Int
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON MyError where
instance FromJSON MyError where

makePrisms ''MyError

instance HasErrorMessage MyError where
  errorMessage MENotSpecified = "Not specified"
  errorMessage MEReasonRequiredForIncomplete = "Reason required when not complete"
  errorMessage MECollectionTooSmall = "Collection too small"
  errorMessage MEFooNotDigits = "Not digits"
  errorMessage MEFooNotLower = "Not lowercase"
  errorMessage MEFooNotUpper = "Not uppercase"
  errorMessage (MEValidity ve) = errorMessage ve
  errorMessage (MEBadWorkflowIndex i) = "Bad workflow index: " <> (Text.pack . show $ i)

instance HasNotSpecified MyError where
  _NotSpecified = _MENotSpecified

instance HasReasonRequiredForIncomplete MyError where
  _ReasonRequiredForIncomplete = _MEReasonRequiredForIncomplete

instance HasCollectionTooSmall MyError where
  _CollectionTooSmall = _MECollectionTooSmall

instance HasFooNotDigits MyError where
  _FooNotDigits = _MEFooNotDigits

instance HasFooNotLower MyError where
  _FooNotLower = _MEFooNotLower

instance HasFooNotUpper MyError where
  _FooNotUpper = _MEFooNotUpper

instance HasValidityError MyError where
  _ValidityError = _MEValidity

instance HasBadWorkflowIndex MyError where
  _BadWorkflowIndex = _MEBadWorkflowIndex

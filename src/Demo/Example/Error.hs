{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Demo.Example.Error (
    MyError(..)
  ) where

import Control.Lens.TH

import Reflex.Dom.Validation

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
  deriving (Eq, Ord, Show, Read)

makePrisms ''MyError

instance HasErrorMessage MyError where
  errorMessage MENotSpecified = "Not specified"
  errorMessage MEReasonRequiredForIncomplete = "Reason required when not complete"
  errorMessage MECollectionTooSmall = "Collection too small"
  errorMessage MEFooNotDigits = "Not digits"
  errorMessage MEFooNotLower = "Not lowercase"
  errorMessage MEFooNotUpper = "Not uppercase"

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

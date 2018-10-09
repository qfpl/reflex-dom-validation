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

data MyError =
    MENotSpecified
  | MEReasonRequiredForIncomplete
  | MECollectionTooSmall
  deriving (Eq, Ord, Show, Read)

makePrisms ''MyError

instance HasErrorMessage MyError where
  errorMessage MENotSpecified = "Not specified"
  errorMessage MEReasonRequiredForIncomplete = "Reason required when not complete"
  errorMessage MECollectionTooSmall = "Collection too small"

instance HasNotSpecified MyError where
  _NotSpecified = _MENotSpecified

instance HasReasonRequiredForIncomplete MyError where
  _ReasonRequiredForIncomplete = _MEReasonRequiredForIncomplete

instance HasCollectionTooSmall MyError where
  _CollectionTooSmall = _MECollectionTooSmall

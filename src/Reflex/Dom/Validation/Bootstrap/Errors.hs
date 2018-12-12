{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.Dom.Validation.Bootstrap.Errors (
    errorClass
  , errorsForId
  ) where

import Control.Monad (void)
import Data.Bool (bool)

import Data.Text (Text)

import Control.Lens

import Reflex.Dom.Core

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Error

errorClass :: Reflex t => Id -> Dynamic t [WithId e] -> Dynamic t Text
errorClass i des =
  bool "is-valid" "is-invalid" . hasMatchingId i <$> des

errorsForId :: (MonadWidget t m, HasErrorMessage e)
            => ValidationWidget t e f u v m ()
errorsForId = toValidationWidget_ $ \i _ _ _ des ->
  let
    dErrors = fmap (errorMessage . view wiValue) . ffilter ((== i) . view wiId) <$> des
  in do
    void . simpleList dErrors $
      divClass "invalid-feedback" . dynText
    pure mempty

{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Demo.Example.Completed (
    AsCompleted(..)
  , completedF
  ) where

import Data.Bool (bool)
import Data.Monoid (Endo(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..))

import Control.Lens

import Reflex.Dom.Core

import Reflex.Dom.Validation

class AsCompleted f where
  completed :: Lens' (f g) (Wrap Bool g)

instance AsCompleted (Wrap Bool) where
  completed = id

completeW :: (MonadWidget t m, HasErrorMessage e)
          => ValidationWidget t m e (Wrap Bool)
completeW i dv des = divClass "form-group" $ do
  el "label" $ text "Complete"

  let
    it = idToText i

  evTrue <- divClass "form-check" $ do
    let
      itt = it <> "-true"
      f = fromMaybe False . unWrap
      dv' = f <$> dv
    iv <- sample . current $ dv'
    let ev = updated dv'

    cb <- checkbox iv $ def
      & setValue .~ ev
      & attributes .~ pure ("id" =: itt)  <>
                    (("class" =:) . ("form-check-input " <>) . bool "is-invalid" "is-valid" . null <$> des)
    elAttr "label" ("class" =: "form-check-label" <> "for" =: itt) $ text "Complete"
    let ev' = cb ^. checkbox_change

    errorsForId i des

    pure $ bool Nothing (Just True) <$> ev'

  evFalse <- divClass "form-check" $ do
    let
      itf = it <> "-false"
      f = maybe False not . unWrap
      dv' = f <$> dv
    iv <- sample . current $ dv'
    let ev = updated dv'

    cb <- checkbox iv $ def
      & setValue .~ ev
      & attributes .~ pure ("id" =: itf)  <>
                    (("class" =:) . ("form-check-input " <>) . bool "is-invalid" "is-valid" . null <$> des)
    elAttr "label" ("class" =: "form-check-label" <> "for" =: itf) $ text "Incomplete"
    let ev' = cb ^. checkbox_change

    errorsForId i des

    pure $ bool Nothing (Just False) <$> ev'


  pure $ Endo . const . Wrap <$> leftmost [evTrue, evFalse]

completedF :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, AsCompleted f)
           => Field t m e f (Wrap Bool)
completedF = Field completed (\i -> Id (Just i) "-c") required completeW

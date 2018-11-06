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

import Reflex.Dom.Validation.Bootstrap.Radio
import Reflex.Dom.Validation.Bootstrap.Errors

class AsCompleted f where
  completed :: Lens' (f g) (Wrap Bool g)

instance AsCompleted (Wrap Bool) where
  completed = id

completeW :: (MonadWidget t m, HasErrorMessage e)
          => ValidationWidget t m e (Wrap Bool)
completeW =
  radioWidget (RadioWidgetConfig (Just "Complete") False
               [ RadioOptionConfig "Complete" "-true" True
               , RadioOptionConfig "Incomplete" "-false" False
               ])

completedF :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, AsCompleted f)
           => Field t m e f (Wrap Bool)
completedF = Field completed (\i -> Id (Just i) "-c") required completeW

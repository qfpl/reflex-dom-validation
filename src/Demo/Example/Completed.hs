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

import Control.Lens

import Reflex.Dom.Core

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Error
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Requires
import Reflex.Dom.Validation.Wrap

import Reflex.Dom.Validation.Bootstrap.Radio

class AsCompleted f where
  completed :: Lens' (f g) (Wrap Bool g)

instance AsCompleted (Wrap Bool) where
  completed = id

completeW :: (MonadWidget t m, HasErrorMessage e)
          => ValidationWidget t m e (Wrap Bool) u ()
completeW =
  radioWidget (RadioWidgetConfig (Just "Complete") False
               [ RadioOptionConfig "Complete" "-true" True
               , RadioOptionConfig "Incomplete" "-false" False
               ]) (SRequired False)

completedF :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, AsCompleted f)
           => Field t m e f (Wrap Bool) u ()
completedF = Field completed united (idApp "-c") required completeW

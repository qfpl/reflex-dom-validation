{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Demo.Example (
    go
  ) where

import Data.Functor.Identity (Identity(..))

import Reflex.Dom.Core

import Reflex.Dom.Validation
import Bootstrap

import Demo.Example.CompletedWithReason
import Demo.Example.TestCollections
import Demo.Example.Error

-- TODO do we always want changes to be saved?
-- - at the moment, any change causes the main dynamic to change
-- - we might want to tie that to buttons in workflows etc..., and so making that configurable might be a thing

go :: forall t m. MonadWidget t m => m (Event t (TestCollections Identity))
go = wrapUp (testCollectionsF :: Field t m MyError TestCollections TestCollections) (TestCollections (CompletedWithReason (Wrap Nothing) (Wrap Nothing)) mempty) $ \d -> do

  -- the version with a validation button
  eValidate <- buttonClass "Validate" "btn"
  pure (current d <@ eValidate)

  -- the version where every change causes a validation
  -- pure (updated d)

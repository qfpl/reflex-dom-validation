{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Dom.Validation.Storage (
    runFieldWidgetWithStorage
  ) where

import Data.Functor.Identity

import Control.Monad.Trans (lift)

import Reflex.Dom.Core

import Data.GADT.Compare (GCompare)

import Reflex.Dom.Storage.Base
import Reflex.Dom.Storage.Class
import Data.GADT.Aeson

import Reflex.Dom.Validation

runFieldWidgetWithStorage :: ( MonadWidget t m
                             , GCompare k
                             , GKey k
                             , FromJSONTag k Identity
                             , ToJSONTag k Identity
                             , NFunctor ty
                             )
                          => StorageType
                          -> k (ty Maybe)
                          -> FieldWidget t m r e ty
                          -> Dynamic t r
                          -> Id
                          -> Event t ()
                          -> m (Dynamic t (ty Maybe), Event t (ty Identity))
runFieldWidgetWithStorage st k fw dr i ev = runStorageT st $ do
  dStorage <- askStorageTagDef k nempty
  iStorage <- sample . current $ dStorage
  let eStorage = updated dStorage

  (ds, es) <- lift $ runFieldWidget fw dr i iStorage eStorage ev

  tellStorageInsert k (updated ds)

  pure (ds, es)

{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
module Demo.Example.TodoItem (
    TodoItem(..)
  , todoItemF
  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.Semigroup (Semigroup(..))

import GHC.Generics (Generic)

import Control.Lens

import Data.Aeson (ToJSON, FromJSON, ToJSON1, FromJSON1)

import Data.Text (Text)

import Data.Validation

import Reflex.Dom.Core

import Reflex.Dom.Validation

import Reflex.Dom.Validation.Bootstrap.Errors

data TodoItem f =
  TodoItem {
    _tiComplete :: Wrap Bool f
  , _tiItem :: Wrap Text f
  } deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup1 f => Semigroup (TodoItem f) where
  TodoItem c1 i1 <> TodoItem c2 i2 = TodoItem (c1 <> c2) (i1 <> i2)

instance Monoid1 f => Monoid (TodoItem f) where
  mempty = TodoItem mempty mempty
  mappend = (<>)

instance ToJSON1 f => ToJSON (TodoItem f) where
instance FromJSON1 f => FromJSON (TodoItem f) where

instance NFunctor TodoItem where
  nmap f (TodoItem c i) = TodoItem (nmap f c) (nmap f i)

makeLenses ''TodoItem

toggleV :: ValidationFn e (Wrap Bool) (Wrap Bool)
toggleV i mv =
  Success . Wrap . Identity . fromMaybe False . unWrap $ mv

toggleW :: (MonadWidget t m)
        => ValidationWidget t m e (Wrap Bool) u
toggleW i dv _ _ = do
  let
    f = fromMaybe False . unWrap
    ev = f <$> updated dv
  iv <- fmap f . sample . current $ dv
  cb <- checkbox iv $ def
    & setValue .~ ev
    & attributes .~ pure ("id" =: idToText i)
  let
    ev' = cb ^. checkbox_change
    eChange = Endo . const . Wrap . Just <$> ev'

  pure $ ValidationWidgetOutput (pure mempty) eChange never

completeF :: MonadWidget t m => Field t m e TodoItem (Wrap Bool) u ()
completeF = Field tiComplete united (idApp "-c") toggleV toggleW

-- TODO make this an error if it is empty
itemV :: ValidationFn e (Wrap Text) (Wrap Text)
itemV _ =
  Success . Wrap . Identity . fromMaybe "" . unWrap

itemW :: MonadWidget t m
      => ValidationWidget t m e (Wrap Text) u
itemW _ dv _ _ = do
  let
    f = fromMaybe "" . unWrap
    ev = f <$> updated dv
  iv <- fmap f . sample . current $ dv
  ti <- textInput $ def
    & setValue .~ ev
    & textInputConfig_initialValue .~ iv
  let
    ev' = ti ^. textInput_input
    eChange = Endo . const . Wrap . Just <$> ev'

  pure $ ValidationWidgetOutput (pure mempty) eChange never

itemF :: MonadWidget t m => Field t m e TodoItem (Wrap Text) u ()
itemF = Field tiItem united (idApp "-i") itemV itemW

todoItemF :: forall t m e u. (MonadWidget t m, HasErrorMessage e) => Field t m e TodoItem TodoItem u u
todoItemF =
  let
    todoItemV i mti =
      TodoItem <$> fieldValidation (completeF @t @m) i mti <*> fieldValidation (itemF @t @m) i mti
    todoItemW i dv du des = do
      ValidationWidgetOutput dCompleteE eComplete eCompleteU <- fieldWidget completeF i dv du des
      ValidationWidgetOutput dItemE eItem eItemU <- fieldWidget itemF i dv du des
      errorsForId i des
      pure $ ValidationWidgetOutput (dCompleteE <> dItemE) (eComplete <> eItem) (eCompleteU <> eItemU)
  in
    Field id id (idApp "-ti") todoItemV todoItemW

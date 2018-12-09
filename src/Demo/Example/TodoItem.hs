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
import Reflex.Dom.Validation.Classes
import Reflex.Dom.Validation.Error
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Wrap

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
toggleV = toValidationFn $ \i ->
  Success . Wrap . Identity . fromMaybe False . unWrap

toggleW :: (MonadWidget t m)
        => ValidationWidget t m e (Wrap Bool) u ()
toggleW = toValidationWidget_ $ \i dv _ _ -> do
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
itemV = toValidationFn $ \_ ->
  Success . Wrap . Identity . fromMaybe "" . unWrap

itemW :: MonadWidget t m
      => ValidationWidget t m e (Wrap Text) u ()
itemW = toValidationWidget_ $ \_ dv _ _ -> do
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
    todoItemV =
      TodoItem <$> 
        fieldValidation (completeF @t @m) <*> 
        fieldValidation (itemF @t @m) 
    todoItemW = do
      fieldWidget completeF 
      fieldWidget itemF 
      errorsForId
  in
    Field id id (idApp "-ti") todoItemV todoItemW

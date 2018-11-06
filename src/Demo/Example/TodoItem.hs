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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module Demo.Example.TodoItem (
    TodoItem(..)
  , todoItemF
  ) where

import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.Semigroup (Semigroup(..))
import Data.Proxy (Proxy(..))

import Control.Lens

import Data.Text (Text)

import Data.Validation

import Reflex.Dom.Core

import Reflex.Dom.Validation

import Reflex.Dom.Validation.Bootstrap.Errors

data TodoItem f =
  TodoItem {
    _tiComplete :: Wrap Bool f
  , _tiItem :: Wrap Text f
  }

makeLenses ''TodoItem

toggleV :: ValidationFn e (Wrap Bool) (Wrap Bool)
toggleV i mv =
  Success . Wrap . Identity . fromMaybe False . unWrap $ mv

toggleW :: (MonadWidget t m)
        => ValidationWidget t m e (Wrap Bool)
toggleW i dv _ = do
  let
    f = fromMaybe False . unWrap
    ev = f <$> updated dv
  iv <- fmap f . sample . current $ dv
  cb <- checkbox iv $ def
    & setValue .~ ev
    & attributes .~ pure ("id" =: idToText i)
  let ev' = cb ^. checkbox_change

  pure $ Endo . const . Wrap . Just <$> ev'

completeF :: MonadWidget t m => Field t m e TodoItem (Wrap Bool)
completeF = Field tiComplete (\i -> Id (Just i) "-c") toggleV toggleW

-- TODO make this an error if it is empty
itemV :: ValidationFn e (Wrap Text) (Wrap Text)
itemV i mv =
  Success . Wrap . Identity . fromMaybe "" . unWrap $ mv

itemW :: MonadWidget t m
      => ValidationWidget t m e (Wrap Text)
itemW i dv _ = do
  let
    f = fromMaybe "" . unWrap
    ev = f <$> updated dv
  iv <- fmap f . sample . current $ dv
  ti <- textInput $ def
    & setValue .~ ev
    & textInputConfig_initialValue .~ iv
  let ev' = ti ^. textInput_input

  pure $ Endo . const . Wrap . Just <$> ev'

itemF :: MonadWidget t m => Field t m e TodoItem (Wrap Text)
itemF = Field tiItem (\i -> Id (Just i) "-i") itemV itemW

todoItemV :: forall t m e. MonadWidget t m => Proxy t -> Proxy m -> ValidationFn e TodoItem TodoItem
todoItemV _ _ i mti =
  TodoItem <$> fieldValidation (completeF @t @m) i mti <*> fieldValidation (itemF @t @m) i mti

todoItemW :: (MonadWidget t m, HasErrorMessage e)
          => ValidationWidget t m e TodoItem
todoItemW i dv des = do

  eComplete <- fieldWidget completeF i dv des
  eItem <- fieldWidget itemF i dv des

  errorsForId i des

  pure $ eComplete <> eItem

todoItemF :: forall t m e. (MonadWidget t m, HasErrorMessage e) => Field t m e TodoItem TodoItem
todoItemF = Field id (\i -> Id (Just i) "-ti") (todoItemV (Proxy :: Proxy t) (Proxy :: Proxy m)) todoItemW

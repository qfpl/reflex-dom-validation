{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
module Demo.Example.TestCollections (
    HasCollectionTooSmall(..)
  , AsTestCollections(..)
  , TestCollections(..)
  , testCollectionsF
  ) where

import Data.Functor.Compose(Compose(..))
import Data.Semigroup (Semigroup(..))
import Data.Proxy (Proxy(..))

import Data.Map (Map)

import Control.Lens

import Data.Validation

import Reflex.Dom.Core

import Reflex.Dom.Validation

import Bootstrap

import Demo.Example.CompletedWithReason
import Demo.Example.TodoItem

data TestCollections f =
  TestCollections {
    _tcCompletedWithReason :: CompletedWithReason f
  , _tcTodoItems :: Map Int (TodoItem f)
  }

makeLenses ''TestCollections

instance AsCompletedWithReason TestCollections where
  completedWithReason = tcCompletedWithReason

instance AsTodoItems TestCollections where
  todoItems = tcTodoItems . _Unwrapped

testCollectionsV :: forall t m e.
                 ( MonadWidget t m
                 , HasErrorMessage e
                 , HasNotSpecified e
                 , HasReasonRequiredForIncomplete e
                 , HasCollectionTooSmall e
                 )
                 => Proxy t
                 -> Proxy m
                 -> ValidationFn e TestCollections TestCollections
testCollectionsV _ _ i tc =
  let
    tf = togglesF @t @m @e @TestCollections
  in
    TestCollections <$>
      fieldValidation (completedWithReasonF @t @m) i tc <*>
      (fieldValidation tf i tc) `bindValidation`
        (\(Compose xs) -> if length xs < 2
                then (Failure . pure . WithId (fieldId tf i) $ _CollectionTooSmall # ())
                else Success xs)

testCollectionsW :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, HasReasonRequiredForIncomplete e)
                 => ValidationWidget t m e TestCollections
testCollectionsW i dv de =
  (<>) <$>
    fieldWidget completedWithReasonF i dv de <*>
    fieldWidget togglesF i dv de

class AsTestCollections f where
  testCollections :: Lens' (f g) (TestCollections g)

instance AsTestCollections TestCollections where
  testCollections = id

testCollectionsF :: forall t m e f. (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, HasReasonRequiredForIncomplete e, HasCollectionTooSmall e, AsTestCollections f)
                     => Field t m e f TestCollections
testCollectionsF =
  Field testCollections (\i -> Id (Just i) "-tc") (testCollectionsV (Proxy :: Proxy t) (Proxy :: Proxy m)) testCollectionsW

class HasCollectionTooSmall e where
  _CollectionTooSmall :: Prism' e ()

class AsTodoItems f where
  todoItems :: Lens' (f g) (Compose (Map Int) TodoItem g)

-- TODO add a text field here to put things through their paces
togglesF :: (MonadWidget t m, HasErrorMessage e, AsTodoItems f) => Field t m e f (Compose (Map Int) TodoItem)
togglesF =
  let
    addMe =
      divClass "form-group" $ do
        ti <- textInput def
        eClick <- buttonClass "Add" "btn"
        pure $ (\t -> TodoItem (Wrap (Just False)) (Wrap (Just t))) <$> current (value ti) <@ eClick
    deleteMe =
      buttonClass "Remove" "btn"
  in
    liftF todoItems (\i -> Id (Just i) "-ts") todoItemF addMe deleteMe

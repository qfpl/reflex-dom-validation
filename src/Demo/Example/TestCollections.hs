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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Demo.Example.TestCollections (
    HasCollectionTooSmall(..)
  , AsTestCollections(..)
  , TestCollections(..)
  , AsTestCollectionsU(..)
  , TestCollectionsU(..)
  , testCollectionsF
  ) where

import Data.Functor.Compose(Compose(..))
import Data.Semigroup (Semigroup(..))

import GHC.Generics (Generic)

import Data.Map (Map)
import qualified Data.Text as Text

import Control.Lens

import Data.Aeson (ToJSON, FromJSON, ToJSON1, FromJSON1)

import Data.Validation

import Reflex.Dom.Core

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Classes
import Reflex.Dom.Validation.Error
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Requires
import Reflex.Dom.Validation.Wrap
import Reflex.Dom.Validation.Collection
import Reflex.Dom.Validation.Html5
import Reflex.Dom.Validation.Workflow

import Bootstrap

import Demo.Example.CompletedWithReason
import Demo.Example.Reason
import Demo.Example.TodoItem
import Demo.Example.Workflow

data TestCollectionsU =
  TestCollectionsU {
    _tcFooU :: FooU
  , _tcTodoItemsU :: Map Int ()
  } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON TestCollectionsU where
instance FromJSON TestCollectionsU where

makeLenses ''TestCollectionsU

instance AsFooU TestCollectionsU where
  fooU = tcFooU

class AsTodoItemsU u where
  todoItemsU :: Lens' u (Map Int ())

instance AsTodoItemsU TestCollectionsU where
  todoItemsU = tcTodoItemsU

data TestCollections f =
  TestCollections {
    _tcCompletedWithReason :: CompletedWithReason f
  , _tcTodoItems :: Map Int (TodoItem f)
  , _tcFoo :: Foo f
  } deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup1 f => Semigroup (TestCollections f) where
  TestCollections c1 t1 f1 <> TestCollections c2 t2 f2 = TestCollections (c1 <> c2) (t1 <> t2) (f1 <> f2)

instance Monoid1 f => Monoid (TestCollections f) where
  mempty = TestCollections mempty mempty mempty
  mappend = (<>)

instance ToJSON1 f => ToJSON (TestCollections f) where
instance FromJSON1 f => FromJSON (TestCollections f) where

instance NFunctor TestCollections where
  nmap g (TestCollections cr ti f) = TestCollections (nmap g cr) (fmap (nmap g) ti) (nmap g f)

makeLenses ''TestCollections

instance AsCompletedWithReason TestCollections where
  completedWithReason = tcCompletedWithReason

instance AsTodoItems TestCollections where
  todoItems = tcTodoItems . _Unwrapped

instance AsFoo TestCollections where
  foo = tcFoo


class AsTestCollections f where
  testCollections :: Lens' (f g) (TestCollections g)

instance AsTestCollections TestCollections where
  testCollections = id

class AsTestCollectionsU u where
  testCollectionsU :: Lens' u TestCollectionsU

instance AsTestCollectionsU TestCollectionsU where
  testCollectionsU = id

testCollectionsF :: forall t m e f u v.
                    ( MonadWidget t m
                    , Eq e
                    , HasErrorMessage e
                    , HasNotSpecified e
                    , HasReasonRequiredForIncomplete e
                    , HasCollectionTooSmall e
                    , HasFooNotDigits e
                    , HasFooNotLower e
                    , HasFooNotUpper e
                    , HasValidityError e
                    , HasBadWorkflowIndex e
                    , AsTestCollections f
                    , AsTestCollectionsU u
                    )
                 => Field t m e f TestCollections u TestCollectionsU v v
testCollectionsF =
  let
    tf =
      togglesF @t @m @e @TestCollections @TestCollectionsU
    testCollectionsV = toValidationFn $ \i c tc ->
      TestCollections <$>
        runValidationFn (fieldValidation (completedWithReasonF @t @m)) i c tc <*>
        ((runValidationFn (fieldValidation tf) i c tc) `bindValidation`
          (\(Compose xs) -> if length xs < 2
                  then (Failure . pure . WithId (fieldId tf i) $ _CollectionTooSmall # ())
                  else Success xs)) <*>
        runValidationFn (fieldValidation (fooF @t @m @e @TestCollections @TestCollectionsU)) i c tc
    testCollectionsW =
        fieldWidget completedWithReasonF >>
        fieldWidget togglesF >>
        fieldWidget fooF
  in
    Field testCollections testCollectionsU (flip const) (idApp "-tc") testCollectionsV testCollectionsW

class HasCollectionTooSmall e where
  _CollectionTooSmall :: Prism' e ()

class AsTodoItems f where
  todoItems :: Lens' (f g) (Compose (Map Int) TodoItem g)

-- TODO add a text field here to put things through their paces
togglesF :: (MonadWidget t m, HasErrorMessage e, AsTodoItems f, AsTodoItemsU u) => Field t m e f (Compose (Map Int) TodoItem) u (Map Int ()) v (Map Int ())
togglesF =
  let
    ki Nothing i = Id (Just i) "-ts"
    ki (Just k) i = Id (Just $ ki Nothing i) . ("-" <>) . Text.pack . show $ k
    addMe =
      divClass "form-group" $ do
        ti <- textInput def
        eClick <- buttonClass "Add" "btn"
        pure $ (\t -> (TodoItem (Wrap (Just False)) (Wrap (Just t)), ())) <$> current (value ti) <@ eClick
    deleteMe =
      buttonClass "Remove" "btn"
  in
    collectionF todoItems todoItemsU (const . fmap (const ()) . getCompose . view todoItems) ki todoItemF addMe deleteMe

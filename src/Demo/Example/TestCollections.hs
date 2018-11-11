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
import qualified Data.Text as Text

import Control.Lens

import Data.Validation

import Reflex.Dom.Core

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Collection
import Reflex.Dom.Validation.Html5

import Bootstrap

import Demo.Example.CompletedWithReason
import Demo.Example.TodoItem
import Demo.Example.Workflow

data TestCollections f =
  TestCollections {
    _tcCompletedWithReason :: CompletedWithReason f
  , _tcTodoItems :: Map Int (TodoItem f)
  , _tcFoo :: Foo f
  }

makeLenses ''TestCollections

instance NFunctor TestCollections where
  nmap g (TestCollections cr ti f) = TestCollections (nmap g cr) (fmap (nmap g) ti) (nmap g f)

instance AsCompletedWithReason TestCollections where
  completedWithReason = tcCompletedWithReason

instance AsTodoItems TestCollections where
  todoItems = tcTodoItems . _Unwrapped

instance AsFoo TestCollections where
  foo = tcFoo

testCollectionsV :: forall t m e.
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
      ((fieldValidation tf i tc) `bindValidation`
        (\(Compose xs) -> if length xs < 2
                then (Failure . pure . WithId (fieldId tf i) $ _CollectionTooSmall # ())
                else Success xs)) <*>
     fieldValidation (fooF @t @m) i tc

testCollectionsW :: ( MonadWidget t m
                    , Eq e
                    , HasErrorMessage e
                    , HasNotSpecified e
                    , HasReasonRequiredForIncomplete e
                    , HasFooNotDigits e
                    , HasFooNotLower e
                    , HasFooNotUpper e
                    , HasValidityError e
                    )
                 => ValidationWidget t m e TestCollections
testCollectionsW i dv de =
  (\x y z -> x <> y <> z) <$>
    fieldWidget completedWithReasonF i dv de <*>
    fieldWidget togglesF i dv de <*>
    fieldWidget fooF i dv de

class AsTestCollections f where
  testCollections :: Lens' (f g) (TestCollections g)

instance AsTestCollections TestCollections where
  testCollections = id

testCollectionsF :: forall t m e f.
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
                    , AsTestCollections f)
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
    ki Nothing i = Id (Just i) "-ts"
    ki (Just k) i = Id (Just $ ki Nothing i) . ("-" <>) . Text.pack . show $ k
    addMe =
      divClass "form-group" $ do
        ti <- textInput def
        eClick <- buttonClass "Add" "btn"
        pure $ (\t -> TodoItem (Wrap (Just False)) (Wrap (Just t))) <$> current (value ti) <@ eClick
    deleteMe =
      buttonClass "Remove" "btn"
  in
    collectionF todoItems ki todoItemF addMe deleteMe

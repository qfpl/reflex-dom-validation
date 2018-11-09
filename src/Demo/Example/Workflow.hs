{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
module Demo.Example.Workflow where

import Data.Char (isDigit, isLower, isUpper)
import Data.Bool (bool)
import Data.Foldable
import Data.Proxy (Proxy(..))

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Validation
import Reflex.Dom.Core

import Bootstrap
import Reflex.Dom.Validation
import Reflex.Dom.Validation.Workflow
import Reflex.Dom.Validation.Bootstrap.Text
import Reflex.Dom.Validation.Bootstrap.Workflow

data Foo f =
  Foo {
    _fooA :: Wrap (Maybe Text) f
  , _fooB :: Wrap (Maybe Text) f
  , _fooC :: Wrap (Maybe Text) f
  }

deriving instance (Eq (f (Maybe Text))) => Eq (Foo f)
deriving instance (Ord (f (Maybe Text))) => Ord (Foo f)
deriving instance (Show (f (Maybe Text))) => Show (Foo f)
deriving instance (Read (f (Maybe Text))) => Read (Foo f)

makeLenses ''Foo

instance NFunctor Foo where
  nmap f (Foo a b c) = Foo (nmap f a) (nmap f b) (nmap f c)

class AsFoo g where
  foo :: Lens' (g f) (Foo f)

instance AsFoo Foo where
  foo = id

class HasFooNotDigits e where
  _FooNotDigits :: Prism' e ()

class HasFooNotLower e where
  _FooNotLower :: Prism' e ()

class HasFooNotUpper e where
  _FooNotUpper :: Prism' e ()

fooAV :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotDigits e)
      => Proxy t -> Proxy m -> ValidationFn e (Wrap (Maybe Text)) (Wrap (Maybe Text))
fooAV _ _ i (Wrap (Just (Just t))) =
  if all isDigit (Text.unpack t)
  then Success . Wrap . Identity . Just $ t
  else Failure . pure . WithId i $ _FooNotDigits # ()
fooAV _ _ _ _ =
  Success . Wrap . Identity $ Nothing

fooAW :: (MonadWidget t m, HasErrorMessage e)
      => ValidationWidget t m e (Wrap (Maybe Text))
fooAW =
  textWidget (TextWidgetConfig (Just "A") UpdateOnChange)

fooAF :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotDigits e)
      => Field t m e Foo (Wrap (Maybe Text))
fooAF =
  Field fooA (\i -> Id (Just i) "-a") (fooAV (Proxy :: Proxy t) (Proxy :: Proxy m)) fooAW

fooBV :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotLower e)
      => Proxy t -> Proxy m -> ValidationFn e (Wrap (Maybe Text)) (Wrap (Maybe Text))
fooBV _ _ i (Wrap (Just (Just t))) =
  if all isLower (Text.unpack t)
  then Success . Wrap . Identity . Just $ t
  else Failure . pure . WithId i $ _FooNotLower # ()
fooBV _ _ _ _ =
  Success . Wrap . Identity $ Nothing

fooBW :: (MonadWidget t m, HasErrorMessage e)
      => ValidationWidget t m e (Wrap (Maybe Text))
fooBW =
  textWidget (TextWidgetConfig (Just "B") UpdateOnChange)

fooBF :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotLower e)
      => Field t m e Foo (Wrap (Maybe Text))
fooBF =
  Field fooB (\i -> Id (Just i) "-b") (fooBV (Proxy :: Proxy t) (Proxy :: Proxy m)) fooBW

fooCV :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotUpper e)
      => Proxy t -> Proxy m -> ValidationFn e (Wrap (Maybe Text)) (Wrap (Maybe Text))
fooCV _ _ i (Wrap (Just (Just t))) =
  if all isUpper (Text.unpack t)
  then Success . Wrap . Identity . Just $ t
  else Failure . pure . WithId i $ _FooNotUpper # ()
fooCV _ _ _ _ =
  Success . Wrap . Identity $ Nothing

fooCW :: (MonadWidget t m, HasErrorMessage e)
      => ValidationWidget t m e (Wrap (Maybe Text))
fooCW =
  textWidget (TextWidgetConfig (Just "C") UpdateOnChange)

fooCF :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotUpper e)
      => Field t m e Foo (Wrap (Maybe Text))
fooCF =
  Field fooC (\i -> Id (Just i) "-c") (fooCV (Proxy :: Proxy t) (Proxy :: Proxy m)) fooCW

fooV :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotDigits e, HasFooNotLower e, HasFooNotUpper e)
     => Proxy t -> Proxy m -> ValidationFn e Foo Foo
fooV _ _ i cr =
  Foo <$>
    fieldValidation (fooAF @t @m) i cr <*>
    fieldValidation (fooBF @t @m) i cr <*>
    fieldValidation (fooCF @t @m) i cr

fooW :: (MonadWidget t m, Eq e, HasErrorMessage e, HasFooNotDigits e, HasFooNotLower e, HasFooNotUpper e)
      => ValidationWidget t m e Foo
fooW =
  workflowWidget
    [ WorkflowStep "Workflow - Page 1" fooAF
    , WorkflowStep "Workflow - Page 2" fooBF
    , WorkflowStep "Workflow - Page 3" fooCF
    ]
    workflowWidgetConfig

fooF :: forall t m e f. (MonadWidget t m, Eq e, HasErrorMessage e, HasFooNotDigits e, HasFooNotLower e, HasFooNotUpper e, AsFoo f)
      => Field t m e f Foo
fooF =
  Field foo (\i -> Id (Just i) "-foo") (fooV (Proxy :: Proxy t) (Proxy :: Proxy m)) fooW

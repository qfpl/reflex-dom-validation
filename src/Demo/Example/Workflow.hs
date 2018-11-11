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
import Data.Monoid (Endo(..))
import Data.Proxy (Proxy(..))
import Text.Read (readMaybe)

import Control.Lens

import Data.Time.Calendar
import Data.Time.Format

import Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.List.NonEmpty as NonEmpty

import Data.Validation
import Reflex.Dom.Core

import Bootstrap
import Reflex.Dom.Validation
import Reflex.Dom.Validation.Html5
import Reflex.Dom.Validation.Workflow
import Reflex.Dom.Validation.Bootstrap.Text
import Reflex.Dom.Validation.Bootstrap.Workflow
import Reflex.Dom.Validation.Bootstrap.Errors

data Foo f =
  Foo {
    _fooA :: Wrap Day f
  , _fooB :: Wrap (Maybe Text) f
  , _fooC :: Wrap (Maybe Text) f
  }

deriving instance (Eq (f Day), Eq (f (Maybe Text))) => Eq (Foo f)
deriving instance (Ord (f Day), Ord (f (Maybe Text))) => Ord (Foo f)
deriving instance (Show (f Day), Show (f (Maybe Text))) => Show (Foo f)
deriving instance (Read (f Day), Read (f (Maybe Text))) => Read (Foo f)

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

-- fooAV :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotDigits e)
--       => Proxy t
--       -> Proxy m
--       -> ValidationFn e (Wrap (Maybe Text)) (Wrap (Maybe Text))
-- fooAV _ _ i (Wrap (Just (Just t))) =
--   if all isDigit (Text.unpack t)
--   then Success . Wrap . Identity . Just $ t
--   else Failure . pure . WithId i $ _FooNotDigits # ()
-- fooAV _ _ _ _ =
--   Success . Wrap . Identity $ Nothing

-- fooAW :: (MonadWidget t m, HasErrorMessage e)
--       => ValidationWidget t m e (Wrap (Maybe Text))
-- fooAW =
--   textWidget (TextWidgetConfig (Just "A") UpdateOnChange)

-- fooAF :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotDigits e)
--       => Field t m e Foo (Wrap (Maybe Text))
-- fooAF =
--   Field fooA (\i -> Id (Just i) "-a") (fooAV (Proxy :: Proxy t) (Proxy :: Proxy m)) fooAW

fooAV :: forall t m e.
         ( MonadWidget t m
         , HasErrorMessage e
         , HasFooNotDigits e
         )
      => Proxy t
      -> Proxy m
      -> ValidationFn e (Wrap Day) (Wrap Day)
fooAV _ _ _ (Wrap (Just x)) =
  Success . Wrap . Identity $ x
fooAV _ _ i (Wrap Nothing) =
  Failure . pure . WithId i $ _FooNotDigits # ()

fooAW :: ( MonadWidget t m
         , HasErrorMessage e
         , HasValidityError e
         )
      => ValidationWidget t m e (Wrap Day)
fooAW i dv des =
  -- textWidget (TextWidgetConfig (Just "A") UpdateOnChange)
  divClass "form-group" $ do
    iv <- sample . current $ dv
    vi <- valid $
      ValidInputConfig
        (Text.pack . formatTime defaultTimeLocale "%Y-%m-%d")
        (Success . parseTimeOrError False defaultTimeLocale "%Y-%m-%d" . Text.unpack)
        "date"
        (unWrap iv)
        (fmapMaybe unWrap $ updated dv)
        (pure mempty)
    let
      (eFailure, eSuccess) = fanEither . fmap toEither . viInput $ vi
    dFailure <- holdDyn [] . leftmost $
      [ fmap (WithId i) . NonEmpty.toList <$> eFailure
      , [] <$ eSuccess
      ]

    errorsForId i des
    pure $ ValidationWidgetOutput dFailure (Endo . const . Wrap . Just <$> eSuccess)

fooAF :: forall t m e.
         ( MonadWidget t m
         , HasErrorMessage e
         , HasFooNotDigits e
         , HasValidityError e
         )
      => Field t m e Foo (Wrap Day)
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

fooV :: forall t m e.
        ( MonadWidget t m
        , HasErrorMessage e
        , HasFooNotDigits e
        , HasFooNotLower e
        , HasFooNotUpper e
        , HasValidityError e
        )
     => Proxy t -> Proxy m -> ValidationFn e Foo Foo
fooV _ _ i cr =
  Foo <$>
    fieldValidation (fooAF @t @m) i cr <*>
    fieldValidation (fooBF @t @m) i cr <*>
    fieldValidation (fooCF @t @m) i cr

fooW :: ( MonadWidget t m
        , Eq e
        , HasErrorMessage e
        , HasFooNotDigits e
        , HasFooNotLower e
        , HasFooNotUpper e
        , HasValidityError e
        )
      => ValidationWidget t m e Foo
fooW =
  workflowWidget
    [ WorkflowStep "Workflow - Page 1" fooAF
    , WorkflowStep "Workflow - Page 2" fooBF
    , WorkflowStep "Workflow - Page 3" fooCF
    ]
    workflowWidgetConfig

fooF :: forall t m e f.
        ( MonadWidget t m
        , Eq e
        , HasErrorMessage e
        , HasFooNotDigits e
        , HasFooNotLower e
        , HasFooNotUpper e
        , HasValidityError e
        , AsFoo f)
      => Field t m e f Foo
fooF =
  Field foo (\i -> Id (Just i) "-foo") (fooV (Proxy :: Proxy t) (Proxy :: Proxy m)) fooW

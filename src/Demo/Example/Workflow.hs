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
import Control.Error

import Data.Time.Calendar
import Data.Colour

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.List.NonEmpty as NonEmpty

import Data.Validation
import Reflex.Dom.Core

import Bootstrap
import Reflex.Dom.Validation
import Reflex.Dom.Validation.Html5
import Reflex.Dom.Validation.Workflow
import Reflex.Dom.Validation.Bootstrap.Text
import Reflex.Dom.Validation.Bootstrap.Workflow
import Reflex.Dom.Validation.Bootstrap.Checkbox
import Reflex.Dom.Validation.Bootstrap.Dropdown
import Reflex.Dom.Validation.Bootstrap.Errors
import Reflex.Dom.Validation.Bootstrap.Html5

data Bar = A | B | C deriving (Eq, Ord, Show, Read)

data SelectDemo f =
  SelectDemo {
    _sdOne :: Wrap Bar f
  , _sdMaybe :: Wrap (Maybe Bar) f
  , _sdMultiple :: Wrap (Set Bar) f
  }

deriving instance (Eq (f Bar), Eq (f (Maybe Bar)), Eq (f (Set Bar))) => Eq (SelectDemo f)
deriving instance (Ord (f Bar), Ord (f (Maybe Bar)), Ord (f (Set Bar))) => Ord (SelectDemo f)
deriving instance (Show (f Bar), Show (f (Maybe Bar)), Show (f (Set Bar))) => Show (SelectDemo f)
deriving instance (Read (f Bar), Read (f (Maybe Bar)), Read (f (Set Bar))) => Read (SelectDemo f)

makeLenses ''SelectDemo

class AsSelectDemo g where
  selectDemo :: Lens' (g f) (SelectDemo f)

instance AsSelectDemo SelectDemo where
  selectDemo = id

instance NFunctor SelectDemo where
  nmap f (SelectDemo o ma my) = SelectDemo (nmap f o) (nmap f ma) (nmap f my)

data Foo f =
  Foo {
    _fooD :: SelectDemo f
  , _fooA :: Wrap Day f
  , _fooB :: Wrap (Set Bar) f
  , _fooC :: Wrap (Maybe Text) f
  }

deriving instance (Eq (SelectDemo f), Eq (f Day), Eq (f (Set Bar)), Eq (f (Maybe Text))) => Eq (Foo f)
deriving instance (Ord (SelectDemo f), Ord (f Day), Ord (f (Set Bar)), Ord (f (Maybe Text))) => Ord (Foo f)
deriving instance (Show (SelectDemo f), Show (f Day), Show (f (Set Bar)), Show (f (Maybe Text))) => Show (Foo f)
deriving instance (Read (SelectDemo f), Read (f Day), Read (f (Set Bar)), Read (f (Maybe Text))) => Read (Foo f)

makeLenses ''Foo

instance NFunctor Foo where
  nmap f (Foo d a b c) = Foo (nmap f d) (nmap f a) (nmap f b) (nmap f c)

class AsFoo g where
  foo :: Lens' (g f) (Foo f)

instance AsFoo Foo where
  foo = id

instance AsSelectDemo Foo where
  selectDemo = fooD

class HasFooNotDigits e where
  _FooNotDigits :: Prism' e ()

class HasFooNotLower e where
  _FooNotLower :: Prism' e ()

class HasFooNotUpper e where
  _FooNotUpper :: Prism' e ()

fooAV :: forall t m e.
         ( MonadWidget t m
         , HasErrorMessage e
         , HasFooNotDigits e
         , HasValidityError e
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
fooAW =
  validWidget $ ValidWidgetConfig (Just "A") dayConfigBuilder

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
      => Proxy t -> Proxy m -> ValidationFn e (Wrap (Set Bar)) (Wrap (Set Bar))
fooBV _ _ _ (Wrap (Just t)) =
  Success . Wrap . Identity $ t
fooBV _ _ _ _ =
  Success . Wrap . Identity $ mempty

fooBW :: (MonadWidget t m, HasErrorMessage e)
      => ValidationWidget t m e (Wrap (Set Bar))
fooBW =
  checkboxWidget . CheckboxWidgetConfig (Just "B") True $
    [ CheckboxOptionConfig "A" "-a" A
    , CheckboxOptionConfig "B" "-b" B
    , CheckboxOptionConfig "C" "-c" C
    ]

fooBF :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotLower e)
      => Field t m e Foo (Wrap (Set Bar))
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

selectOV :: (MonadWidget t m, HasNotSpecified e) => Proxy t -> Proxy m -> ValidationFn e (Wrap Bar) (Wrap Bar)
selectOV _ _ i (Wrap (Just x)) = Success . Wrap . Identity $ x
selectOV _ _ i (Wrap Nothing) = Failure . pure . WithId i $ _NotSpecified # ()

selectOW :: (MonadWidget t m, HasErrorMessage e)
         => ValidationWidget t m e (Wrap Bar)
selectOW =
  selectWidget . SelectWidgetConfig (Just "One") $
    [ SelectOptionConfig "A" A
    , SelectOptionConfig "B" B
    , SelectOptionConfig "C" C
    ]

selectOF :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasNotSpecified e)
         => Field t m e SelectDemo (Wrap Bar)
selectOF =
  Field sdOne (\i -> Id (Just i) "-o") (selectOV (Proxy :: Proxy t) (Proxy :: Proxy m)) selectOW

selectMaV :: MonadWidget t m => Proxy t -> Proxy m -> ValidationFn e (Wrap (Maybe Bar)) (Wrap (Maybe Bar))
selectMaV _ _ i (Wrap (Just mb)) = Success . Wrap . Identity $ mb
selectMaV _ _ i (Wrap Nothing) = Success . Wrap . Identity $ Nothing

selectMaW :: (MonadWidget t m, HasErrorMessage e)
          => ValidationWidget t m e (Wrap (Maybe Bar))
selectMaW =
  selectOptionalWidget . SelectWidgetConfig (Just "Maybe") $
    [ SelectOptionConfig "A" A
    , SelectOptionConfig "B" B
    , SelectOptionConfig "C" C
    ]

selectMaF :: forall t m e. (MonadWidget t m, HasErrorMessage e) => Field t m e SelectDemo (Wrap (Maybe Bar))
selectMaF =
  Field sdMaybe (\i -> Id (Just i) "-ma") (selectMaV (Proxy :: Proxy t) (Proxy :: Proxy m)) selectMaW

selectMuV :: MonadWidget t m => Proxy t -> Proxy m -> ValidationFn e (Wrap (Set Bar)) (Wrap (Set Bar))
selectMuV _ _ i (Wrap (Just s)) = Success . Wrap . Identity $ s
selectMuV _ _ i (Wrap Nothing) = Success . Wrap . Identity $ mempty

selectMuW :: (MonadWidget t m, HasErrorMessage e)
          => ValidationWidget t m e (Wrap (Set Bar))
selectMuW =
  selectMultipleWidget . SelectWidgetConfig (Just "Multiple") $
    [ SelectOptionConfig "A" A
    , SelectOptionConfig "B" B
    , SelectOptionConfig "C" C
    ]

selectMuF :: forall t m e. (MonadWidget t m, HasErrorMessage e) => Field t m e SelectDemo (Wrap (Set Bar))
selectMuF =
  Field sdMultiple (\i -> Id (Just i) "-mu") (selectMuV (Proxy :: Proxy t) (Proxy :: Proxy m)) selectMuW

fooDV :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotUpper e, HasNotSpecified e)
      => Proxy t -> Proxy m -> ValidationFn e SelectDemo SelectDemo
fooDV _ _ i cr =
  SelectDemo <$>
    fieldValidation (selectOF @t @m) i cr <*>
    fieldValidation (selectMaF @t @m) i cr <*>
    fieldValidation (selectMuF @t @m) i cr

fooDW :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e)
      => ValidationWidget t m e SelectDemo
fooDW i dv des = do
  eO <- fieldWidget selectOF i dv des
  eMa <- fieldWidget selectMaF i dv des
  eMu <- fieldWidget selectMuF i dv des
  pure $ eO <> eMa <> eMu

fooDF :: forall t m e f. (MonadWidget t m, AsSelectDemo f, HasErrorMessage e, HasFooNotUpper e, HasNotSpecified e)
      => Field t m e f SelectDemo
fooDF =
  Field selectDemo (\i -> Id (Just i) "-d") (fooDV (Proxy :: Proxy t) (Proxy :: Proxy m)) fooDW

fooV :: forall t m e.
        ( MonadWidget t m
        , HasErrorMessage e
        , HasFooNotDigits e
        , HasFooNotLower e
        , HasFooNotUpper e
        , HasValidityError e
        , HasNotSpecified e
        )
     => Proxy t -> Proxy m -> ValidationFn e Foo Foo
fooV _ _ i cr =
  Foo <$>
    fieldValidation (fooDF @t @m) i cr <*>
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
        , HasNotSpecified e
        )
      => ValidationWidget t m e Foo
fooW =
  workflowWidget
    [ WorkflowStep "Workflow - Page 1" fooDF
    , WorkflowStep "Workflow - Page 2" fooAF
    , WorkflowStep "Workflow - Page 3" fooBF
    , WorkflowStep "Workflow - Page 4" fooCF
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
        , HasNotSpecified e
        , AsFoo f)
      => Field t m e f Foo
fooF =
  Field foo (\i -> Id (Just i) "-foo") (fooV (Proxy :: Proxy t) (Proxy :: Proxy m)) fooW

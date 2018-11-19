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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
module Demo.Example.Workflow where

import Control.Monad (join)
import Data.Char (isDigit, isLower, isUpper)
import Data.Bool (bool)
import Data.Foldable
import Data.Functor.Classes
import Data.Monoid (Endo(..))
import Data.Proxy (Proxy(..))
import Text.Read (readMaybe)

import GHC.Generics (Generic)

import Control.Lens
import Control.Error

import Data.Time.Calendar
import Data.Colour

import Data.Aeson (ToJSON, FromJSON, ToJSON1, FromJSON1)

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
import Reflex.Dom.Validation.Bootstrap.Select
import Reflex.Dom.Validation.Bootstrap.Errors
import Reflex.Dom.Validation.Bootstrap.Html5

data Nest1 f =
  Nest1 {
    _n1a :: Wrap (Maybe Text) f
  , _n1b :: Wrap (Maybe Text) f
  , _n1c :: Wrap (Maybe Text) f
  } deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup1 f => Semigroup (Nest1 f) where
  Nest1 a1 b1 c1 <> Nest1 a2 b2 c2 = Nest1 (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid1 f => Monoid (Nest1 f) where
  mempty = Nest1 mempty mempty mempty
  mappend = (<>)

instance ToJSON1 f => ToJSON (Nest1 f) where
instance FromJSON1 f => FromJSON (Nest1 f) where

instance NFunctor Nest1 where
  nmap z (Nest1 a b c) = Nest1 (nmap z a) (nmap z b) (nmap z c)

makeLenses ''Nest1

data Nest2 f =
  Nest2 {
    _n2d :: Wrap (Maybe Text) f
  , _n2e :: Wrap (Maybe Text) f
  , _n2f :: Wrap (Maybe Text) f
  } deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup1 f => Semigroup (Nest2 f) where
  Nest2 a1 b1 c1 <> Nest2 a2 b2 c2 = Nest2 (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid1 f => Monoid (Nest2 f) where
  mempty = Nest2 mempty mempty mempty
  mappend = (<>)

instance ToJSON1 f => ToJSON (Nest2 f) where
instance FromJSON1 f => FromJSON (Nest2 f) where

instance NFunctor Nest2 where
  nmap z (Nest2 a b c) = Nest2 (nmap z a) (nmap z b) (nmap z c)

makeLenses ''Nest2

data Nest3 f =
  Nest3 {
    _n3g :: Wrap (Maybe Text) f
  , _n3h :: Wrap (Maybe Text) f
  , _n3i :: Wrap (Maybe Text) f
  } deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup1 f => Semigroup (Nest3 f) where
  Nest3 a1 b1 c1 <> Nest3 a2 b2 c2 = Nest3 (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid1 f => Monoid (Nest3 f) where
  mempty = Nest3 mempty mempty mempty
  mappend = (<>)

instance ToJSON1 f => ToJSON (Nest3 f) where
instance FromJSON1 f => FromJSON (Nest3 f) where

instance NFunctor Nest3 where
  nmap z (Nest3 a b c) = Nest3 (nmap z a) (nmap z b) (nmap z c)

makeLenses ''Nest3

data Nest f =
  Nest {
    _n1 :: Nest1 f
  , _n2 :: Nest2 f
  , _n3 :: Nest3 f
  } deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup1 f => Semigroup (Nest f) where
  Nest a1 b1 c1 <> Nest a2 b2 c2 = Nest (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid1 f => Monoid (Nest f) where
  mempty = Nest mempty mempty mempty
  mappend = (<>)

instance ToJSON1 f => ToJSON (Nest f) where
instance FromJSON1 f => FromJSON (Nest f) where

instance NFunctor Nest where
  nmap z (Nest a b c) = Nest (nmap z a) (nmap z b) (nmap z c)

makeLenses ''Nest

fooNVx :: forall t m e. (MonadWidget t m)
      => Proxy t -> Proxy m -> ValidationFn e (Wrap (Maybe Text)) (Wrap (Maybe Text))
fooNVx _ _ _ (Wrap mt) =
  Success . Wrap . Identity . join $ mt

fooNWx :: (MonadWidget t m, HasErrorMessage e)
      => Text
      -> ValidationWidget t m e (Wrap (Maybe Text))
fooNWx l =
  textWidget (TextWidgetConfig (Just l) UpdateOnChange)

fooNFx :: forall t m e f. (MonadWidget t m, HasErrorMessage e)
      => (forall g. Lens' (f g) (Wrap (Maybe Text) g))
      -> Text
      -> Text
      -> Field t m e f (Wrap (Maybe Text))
fooNFx o i l =
  Field o (\p -> Id (Just p) i) (fooNVx (Proxy :: Proxy t) (Proxy :: Proxy m)) (fooNWx l)

fooN1a :: (MonadWidget t m, HasErrorMessage e) => Field t m e Nest1 (Wrap (Maybe Text))
fooN1a = fooNFx n1a "-a" "A"

fooN1b :: (MonadWidget t m, HasErrorMessage e) => Field t m e Nest1 (Wrap (Maybe Text))
fooN1b = fooNFx n1b "-b" "B"

fooN1c :: (MonadWidget t m, HasErrorMessage e) => Field t m e Nest1 (Wrap (Maybe Text))
fooN1c = fooNFx n1c "-c" "C"

fooN2d :: (MonadWidget t m, HasErrorMessage e) => Field t m e Nest2 (Wrap (Maybe Text))
fooN2d = fooNFx n2d "-d" "D"

fooN2e :: (MonadWidget t m, HasErrorMessage e) => Field t m e Nest2 (Wrap (Maybe Text))
fooN2e = fooNFx n2e "-e" "E"

fooN2f :: (MonadWidget t m, HasErrorMessage e) => Field t m e Nest2 (Wrap (Maybe Text))
fooN2f = fooNFx n2f "-f" "F"

fooN3g :: (MonadWidget t m, HasErrorMessage e) => Field t m e Nest3 (Wrap (Maybe Text))
fooN3g = fooNFx n3g "-g" "G"

fooN3h :: (MonadWidget t m, HasErrorMessage e) => Field t m e Nest3 (Wrap (Maybe Text))
fooN3h = fooNFx n3h "-h" "H"

fooN3i :: (MonadWidget t m, HasErrorMessage e) => Field t m e Nest3 (Wrap (Maybe Text))
fooN3i = fooNFx n3i "-i" "I"

fooN1V :: forall t m e. (MonadWidget t m, HasErrorMessage e)
       => Proxy t
       -> Proxy m
       -> ValidationFn e Nest1 Nest1
fooN1V _ _ i v =
  Nest1 <$>
    fieldValidation (fooN1a @t @m) i v <*>
    fieldValidation (fooN1b @t @m) i v <*>
    fieldValidation (fooN1c @t @m) i v

fooN1W :: (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e)
       => ValidationWidget t m e Nest1
fooN1W = workflowWidget [ WorkflowStep "W1" fooN1a
                        , WorkflowStep "W2" fooN1b
                        , WorkflowStep "W3" fooN1c
                        ] workflowWidgetConfig

fooN1F :: forall t m e. (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e)
      => Field t m e Nest Nest1
fooN1F =
  Field n1 (\i -> Id (Just i) "-1") (fooN1V (Proxy :: Proxy t) (Proxy :: Proxy m)) fooN1W

fooN2V :: forall t m e. (MonadWidget t m, HasErrorMessage e)
       => Proxy t
       -> Proxy m
       -> ValidationFn e Nest2 Nest2
fooN2V _ _ i v =
  Nest2 <$>
    fieldValidation (fooN2d @t @m) i v <*>
    fieldValidation (fooN2e @t @m) i v <*>
    fieldValidation (fooN2f @t @m) i v

fooN2W :: (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e)
       => ValidationWidget t m e Nest2
fooN2W = workflowWidget [ WorkflowStep "W4" fooN2d
                        , WorkflowStep "W5" fooN2e
                        , WorkflowStep "W6" fooN2f
                        ] workflowWidgetConfig

fooN2F :: forall t m e. (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e)
      => Field t m e Nest Nest2
fooN2F =
  Field n2 (\i -> Id (Just i) "-2") (fooN2V (Proxy :: Proxy t) (Proxy :: Proxy m)) fooN2W

fooN3V :: forall t m e. (MonadWidget t m, HasErrorMessage e)
       => Proxy t
       -> Proxy m
       -> ValidationFn e Nest3 Nest3
fooN3V _ _ i v =
  Nest3 <$>
    fieldValidation (fooN3g @t @m) i v <*>
    fieldValidation (fooN3h @t @m) i v <*>
    fieldValidation (fooN3i @t @m) i v

fooN3W :: (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e)
       => ValidationWidget t m e Nest3
fooN3W = workflowWidget [ WorkflowStep "W7" fooN3g
                        , WorkflowStep "W8" fooN3h
                        , WorkflowStep "W9" fooN3i
                        ] workflowWidgetConfig

fooN3F :: forall t m e. (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e)
      => Field t m e Nest Nest3
fooN3F =
  Field n3 (\i -> Id (Just i) "-3") (fooN3V (Proxy :: Proxy t) (Proxy :: Proxy m)) fooN3W

fooNV :: forall t m e. (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e)
      => Proxy t
      -> Proxy m
      -> ValidationFn e Nest Nest
fooNV _ _ i v =
  Nest <$>
    fieldValidation (fooN1F @t @m) i v <*>
    fieldValidation (fooN2F @t @m) i v <*>
    fieldValidation (fooN3F @t @m) i v

fooNW :: (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e)
       => ValidationWidget t m e Nest
fooNW = workflowWidget [ WorkflowStep "WA" fooN1F
                        , WorkflowStep "WB" fooN2F
                        , WorkflowStep "WC" fooN3F
                        ] workflowWidgetConfig

fooNF :: forall t m e. (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e)
      => Field t m e Nest Nest
fooNF =
  Field id (\i -> Id (Just i) "-n") (fooNV (Proxy :: Proxy t) (Proxy :: Proxy m)) fooNW

data Bar = A | B | C deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Bar where
instance FromJSON Bar where

data SelectDemo f =
  SelectDemo {
    _sdOne :: Wrap Bar f
  , _sdMaybe :: Wrap (Maybe Bar) f
  -- , _sdMultiple :: Wrap (Set Bar) f
  } deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup1 f => Semigroup (SelectDemo f) where
  SelectDemo o1 m1 <> SelectDemo o2 m2 = SelectDemo (o1 <> o2) (m1 <> m2)

instance Monoid1 f => Monoid (SelectDemo f) where
  mempty = SelectDemo mempty mempty
  mappend = (<>)

instance ToJSON1 f => ToJSON (SelectDemo f) where
instance FromJSON1 f => FromJSON (SelectDemo f) where

instance NFunctor SelectDemo where
  nmap f (SelectDemo o ma)  = SelectDemo (nmap f o) (nmap f ma)

makeLenses ''SelectDemo

class AsSelectDemo g where
  selectDemo :: Lens' (g f) (SelectDemo f)

instance AsSelectDemo SelectDemo where
  selectDemo = id

data Foo f =
  Foo {
    _fooD :: SelectDemo f
  , _fooA :: Wrap Day f
  , _fooB :: Wrap (Set Bar) f
  , _fooC :: Wrap (Maybe Text) f
  } deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup1 f => Semigroup (Foo f) where
  Foo d1 a1 b1 c1 <> Foo d2 a2 b2 c2 = Foo (d1 <> d2) (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid1 f => Monoid (Foo f) where
  mempty = Foo mempty mempty mempty mempty
  mappend = (<>)

instance ToJSON1 f => ToJSON (Foo f) where
instance FromJSON1 f => FromJSON (Foo f) where

instance NFunctor Foo where
  nmap f (Foo d a b c) = Foo (nmap f d) (nmap f a) (nmap f b) (nmap f c)

makeLenses ''Foo

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
  selectWidget A . SelectWidgetConfig (Just "One") $
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

fooDV :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasFooNotUpper e, HasNotSpecified e)
      => Proxy t -> Proxy m -> ValidationFn e SelectDemo SelectDemo
fooDV _ _ i cr =
  SelectDemo <$>
    fieldValidation (selectOF @t @m) i cr <*>
    fieldValidation (selectMaF @t @m) i cr

fooDW :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e)
      => ValidationWidget t m e SelectDemo
fooDW i dv des = do
  eO <- fieldWidget selectOF i dv des
  eMa <- fieldWidget selectMaF i dv des
  pure $ eO <> eMa

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
        , HasBadWorkflowIndex e
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
        , HasBadWorkflowIndex e
        , AsFoo f)
      => Field t m e f Foo
fooF =
  Field foo (\i -> Id (Just i) "-foo") (fooV (Proxy :: Proxy t) (Proxy :: Proxy m)) fooW

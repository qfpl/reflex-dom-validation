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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
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
import Text.Read (readMaybe)

import GHC.Generics (Generic)

import Control.Lens

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
import Reflex.Dom.Validation.Classes
import Reflex.Dom.Validation.Error
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Requires
import Reflex.Dom.Validation.Wrap
import Reflex.Dom.Validation.Html5
import Reflex.Dom.Validation.Workflow
import Reflex.Dom.Validation.Bootstrap.Text
import Reflex.Dom.Validation.Bootstrap.Workflow
import Reflex.Dom.Validation.Bootstrap.Checkbox
import Reflex.Dom.Validation.Bootstrap.Select
import Reflex.Dom.Validation.Bootstrap.Errors
import Reflex.Dom.Validation.Bootstrap.Html5

data Nest1U =
  Nest1U {
    _n1uIx :: Int
  } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Nest1U where
instance FromJSON Nest1U where

makeLenses ''Nest1U

instance AsWorkflowIndex Nest1U where
  workflowIndex = n1uIx

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

data Nest2U =
  Nest2U {
    _n2uIx :: Int
  } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Nest2U where
instance FromJSON Nest2U where

makeLenses ''Nest2U

instance AsWorkflowIndex Nest2U where
  workflowIndex = n2uIx

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

data Nest3U =
  Nest3U {
    _n3uIx :: Int
  } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Nest3U where
instance FromJSON Nest3U where

makeLenses ''Nest3U

instance AsWorkflowIndex Nest3U where
  workflowIndex = n3uIx

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

data NestU =
  NestU {
    _nuIx :: Int
  , _n1u :: Nest1U
  , _n2u :: Nest2U
  , _n3u :: Nest3U
  } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON NestU where
instance FromJSON NestU where

makeLenses ''NestU

instance AsWorkflowIndex NestU where
  workflowIndex = nuIx

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

fooNVx :: HasNotSpecified e
      => ValidationFn e (Wrap (Maybe Text)) (Wrap (Maybe Text))
fooNVx = toValidationFn $ \i v ->
  case v of
    Wrap (Just (Just t)) -> Success . Wrap . Identity . Just $ t
    Wrap Nothing -> Failure . pure . WithId i $ _NotSpecified # ()

fooNWx :: (MonadWidget t m, HasErrorMessage e)
      => Text
      -> ValidationWidget t e (Wrap (Maybe Text)) u m ()
fooNWx l =
  textWidget (TextWidgetConfig (Just l) UpdateOnChange) SOptional

fooNFx :: forall t m e f u. (MonadWidget t m, HasErrorMessage e, HasNotSpecified e)
      => (forall g. Lens' (f g) (Wrap (Maybe Text) g))
      -> Text
      -> Text
      -> Field t m e f (Wrap (Maybe Text)) u ()
fooNFx o i l =
  Field o united (idApp i) fooNVx (fooNWx l)

fooN1a :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e) => Field t m e Nest1 (Wrap (Maybe Text)) u ()
fooN1a = fooNFx n1a "-a" "A"

fooN1b :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e) => Field t m e Nest1 (Wrap (Maybe Text)) u ()
fooN1b = fooNFx n1b "-b" "B"

fooN1c :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e) => Field t m e Nest1 (Wrap (Maybe Text)) u ()
fooN1c = fooNFx n1c "-c" "C"

fooN2d :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e) => Field t m e Nest2 (Wrap (Maybe Text)) u ()
fooN2d = fooNFx n2d "-d" "D"

fooN2e :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e) => Field t m e Nest2 (Wrap (Maybe Text)) u ()
fooN2e = fooNFx n2e "-e" "E"

fooN2f :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e) => Field t m e Nest2 (Wrap (Maybe Text)) u ()
fooN2f = fooNFx n2f "-f" "F"

fooN3g :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e) => Field t m e Nest3 (Wrap (Maybe Text)) u ()
fooN3g = fooNFx n3g "-g" "G"

fooN3h :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e) => Field t m e Nest3 (Wrap (Maybe Text)) u ()
fooN3h = fooNFx n3h "-h" "H"

fooN3i :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e) => Field t m e Nest3 (Wrap (Maybe Text)) u ()
fooN3i = fooNFx n3i "-i" "I"

fooN1F :: forall t m e. (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e, HasNotSpecified e)
      => Field t m e Nest Nest1 NestU Nest1U
fooN1F =
  let
    fooN1V =
      Nest1 <$>
        fieldValidation (fooN1a @t @m) <*>
        fieldValidation (fooN1b @t @m) <*>
        fieldValidation (fooN1c @t @m) 
    fooN1W =
      workflowWidget [ WorkflowStep "W1" fooN1a []
                     , WorkflowStep "W2" fooN1b []
                     , WorkflowStep "W3" fooN1c []
                     ] workflowWidgetConfig
  in
    Field n1 n1u (idApp "-1") fooN1V fooN1W

fooN2F :: forall t m e. (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e, HasNotSpecified e)
      => Field t m e Nest Nest2 NestU Nest2U
fooN2F =
  let
    fooN2V =
      Nest2 <$>
        fieldValidation (fooN2d @t @m) <*>
        fieldValidation (fooN2e @t @m) <*>
        fieldValidation (fooN2f @t @m) 
    fooN2W =
      workflowWidget [ WorkflowStep "W4" fooN2d []
                     , WorkflowStep "W5" fooN2e []
                     , WorkflowStep "W6" fooN2f []
                     ] workflowWidgetConfig
  in
    Field n2 n2u (idApp "-2") fooN2V fooN2W

fooN3F :: forall t m e. (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e, HasNotSpecified e)
      => Field t m e Nest Nest3 NestU Nest3U
fooN3F =
  let
    fooN3V =
      Nest3 <$>
        fieldValidation (fooN3g @t @m) <*>
        fieldValidation (fooN3h @t @m) <*>
        fieldValidation (fooN3i @t @m)
    fooN3W =
      workflowWidget [ WorkflowStep "W7" fooN3g []
                    , WorkflowStep "W8" fooN3h []
                    , WorkflowStep "W9" fooN3i []
                    ] workflowWidgetConfig
  in
    Field n3 n3u (idApp "-3") fooN3V fooN3W

class AsNest f where
  nest :: Lens' (f g) (Nest g)

instance AsNest Nest where
  nest = id

class AsNestU u where
  nestU :: Lens' u NestU

instance AsNestU NestU where
  nestU = id

fooNF :: forall t m e f u. (MonadWidget t m, HasErrorMessage e, Eq e, HasBadWorkflowIndex e, HasNotSpecified e, AsNest f, AsNestU u)
      => Field t m e f Nest u NestU
fooNF =
  let
    fooNV =
      Nest <$>
        fieldValidation (fooN1F @t @m) <*>
        fieldValidation (fooN2F @t @m) <*>
        fieldValidation (fooN3F @t @m) 

    fooNW =
      workflowWidget [ WorkflowStep "WA" fooN1F $
                       [ WorkflowStep "W1" fooN1a []
                       , WorkflowStep "W2" fooN1b []
                       , WorkflowStep "W3" fooN1c []
                       ]
                     , WorkflowStep "WB" fooN2F $
                       [ WorkflowStep "W4" fooN2d []
                       , WorkflowStep "W5" fooN2e []
                       , WorkflowStep "W6" fooN2f []
                       ]
                     , WorkflowStep "WC" fooN3F $
                       [ WorkflowStep "W7" fooN3g []
                       , WorkflowStep "W8" fooN3h []
                       , WorkflowStep "W9" fooN3i []
                       ]
                     ] workflowWidgetConfig
  in
    Field nest nestU (idApp "-n") fooNV fooNW

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

data FooU =
  FooU {
    _fooIx :: Int
  , _fooNest :: NestU
  } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON FooU where
instance FromJSON FooU where

makeLenses ''FooU

instance AsWorkflowIndex FooU where
  workflowIndex = fooIx

instance AsNestU FooU where
  nestU = fooNest

class AsFooU u where
  fooU :: Lens' u FooU

instance AsFooU FooU where
  fooU = id

data Foo f =
  Foo {
    _fooN :: Nest f
  , _fooD :: SelectDemo f
  , _fooA :: Wrap Day f
  , _fooB :: Wrap (Set Bar) f
  , _fooC :: Wrap (Maybe Text) f
  } deriving (Eq, Ord, Show, Read, Generic)

instance Semigroup1 f => Semigroup (Foo f) where
  Foo n1 d1 a1 b1 c1 <> Foo n2 d2 a2 b2 c2 = Foo (n1 <> n2) (d1 <> d2) (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid1 f => Monoid (Foo f) where
  mempty = Foo mempty mempty mempty mempty mempty
  mappend = (<>)

instance ToJSON1 f => ToJSON (Foo f) where
instance FromJSON1 f => FromJSON (Foo f) where

instance NFunctor Foo where
  nmap f (Foo n d a b c) = Foo (nmap f n) (nmap f d) (nmap f a) (nmap f b) (nmap f c)

makeLenses ''Foo

class AsFoo g where
  foo :: Lens' (g f) (Foo f)

instance AsFoo Foo where
  foo = id

instance AsNest Foo where
  nest = fooN

instance AsSelectDemo Foo where
  selectDemo = fooD

class HasFooNotDigits e where
  _FooNotDigits :: Prism' e ()

class HasFooNotLower e where
  _FooNotLower :: Prism' e ()

class HasFooNotUpper e where
  _FooNotUpper :: Prism' e ()

fooAV :: ( HasErrorMessage e
         , HasFooNotDigits e
         , HasValidityError e
         )
      => ValidationFn e (Wrap Day) (Wrap Day)
fooAV = toValidationFn $ \i v -> 
  case v of
    Wrap (Just x) -> Success . Wrap . Identity $ x
    Wrap Nothing -> Failure . pure . WithId i $ _FooNotDigits # ()

fooAW :: ( MonadWidget t m
         , HasErrorMessage e
         , HasValidityError e
         )
      => ValidationWidget t e (Wrap Day) u m ()
fooAW =
  validWidget $ ValidWidgetConfig (Just "A") dayConfigBuilder

fooAF :: ( MonadWidget t m
         , HasErrorMessage e
         , HasFooNotDigits e
         , HasValidityError e
         )
      => Field t m e Foo (Wrap Day) u ()
fooAF =
  Field fooA united (idApp "-a") fooAV fooAW

fooBV :: (HasErrorMessage e, HasFooNotLower e)
      => ValidationFn e (Wrap (Set Bar)) (Wrap (Set Bar))
fooBV = toValidationFn $ \_ v ->
  case v of
    Wrap (Just t) -> Success . Wrap . Identity $ t
    _ -> Success . Wrap . Identity $ mempty

fooBW :: (MonadWidget t m, HasErrorMessage e)
      => ValidationWidget t e (Wrap (Set Bar)) u m ()
fooBW =
  checkboxWidget . CheckboxWidgetConfig (Just "B") True $
    [ CheckboxOptionConfig "A" "-a" A
    , CheckboxOptionConfig "B" "-b" B
    , CheckboxOptionConfig "C" "-c" C
    ]

fooBF :: (MonadWidget t m, HasErrorMessage e, HasFooNotLower e)
      => Field t m e Foo (Wrap (Set Bar)) u ()
fooBF =
  Field fooB united (idApp "-b") fooBV fooBW

fooCV :: (HasErrorMessage e, HasFooNotUpper e)
      => ValidationFn e (Wrap (Maybe Text)) (Wrap (Maybe Text))
fooCV = toValidationFn $ \i v -> 
  case v of
    Wrap (Just (Just t)) ->
      if all isUpper (Text.unpack t)
      then Success . Wrap . Identity . Just $ t
      else Failure . pure . WithId i $ _FooNotUpper # ()
    _ -> 
      Success . Wrap . Identity $ Nothing

fooCW :: (MonadWidget t m, HasErrorMessage e)
      => ValidationWidget t e (Wrap (Maybe Text)) u m ()
fooCW =
  textWidget (TextWidgetConfig (Just "C") UpdateOnChange) SOptional

fooCF :: (MonadWidget t m, HasErrorMessage e, HasFooNotUpper e)
      => Field t m e Foo (Wrap (Maybe Text)) u ()
fooCF =
  Field fooC united (idApp "-c") fooCV fooCW

selectOV :: (HasNotSpecified e)
         => ValidationFn e (Wrap Bar) (Wrap Bar)
selectOV = toValidationFn $ \i v ->
  case v of
    Wrap (Just x) -> Success . Wrap . Identity $ x
    Wrap Nothing -> Failure . pure . WithId i $ _NotSpecified # ()

selectOW :: (MonadWidget t m, HasErrorMessage e)
         => ValidationWidget t e (Wrap Bar) u m ()
selectOW =
  selectWidget A . SelectWidgetConfig (Just "One") $
    [ SelectOptionConfig "A" A
    , SelectOptionConfig "B" B
    , SelectOptionConfig "C" C
    ]

selectOF :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e)
         => Field t m e SelectDemo (Wrap Bar) u ()
selectOF =
  Field sdOne united (idApp "-o") selectOV selectOW

selectMaV :: ValidationFn e (Wrap (Maybe Bar)) (Wrap (Maybe Bar))
selectMaV = toValidationFn $ \i v ->
  case v of
    Wrap (Just mb) -> Success . Wrap . Identity $ mb
    Wrap Nothing -> Success . Wrap . Identity $ Nothing

selectMaW :: (MonadWidget t m, HasErrorMessage e)
          => ValidationWidget t e (Wrap (Maybe Bar)) u m ()
selectMaW =
  selectOptionalWidget . SelectWidgetConfig (Just "Maybe") $
    [ SelectOptionConfig "A" A
    , SelectOptionConfig "B" B
    , SelectOptionConfig "C" C
    ]

selectMaF :: (MonadWidget t m, HasErrorMessage e) => Field t m e SelectDemo (Wrap (Maybe Bar)) u ()
selectMaF =
  Field sdMaybe united (idApp "-ma") selectMaV selectMaW

fooDF :: forall t m e f u. (MonadWidget t m, AsSelectDemo f, HasErrorMessage e, HasFooNotUpper e, HasNotSpecified e)
      => Field t m e f SelectDemo u ()
fooDF =
  let
    fooDV =
      SelectDemo <$>
        fieldValidation (selectOF @t @m) <*>
        fieldValidation (selectMaF @t @m) 

    fooDW =
      fieldWidget selectOF >>
      fieldWidget selectMaF
  in
    Field selectDemo united (idApp "-d") fooDV fooDW

fooF :: forall t m e f u.
        ( MonadWidget t m
        , Eq e
        , HasErrorMessage e
        , HasFooNotDigits e
        , HasFooNotLower e
        , HasFooNotUpper e
        , HasValidityError e
        , HasNotSpecified e
        , HasBadWorkflowIndex e
        , AsFoo f
        , AsFooU u
        )
      => Field t m e f Foo u FooU
fooF =
  let
    fooV =
      Foo <$>
        fieldValidation (fooNF @t @m @e @Foo @FooU) <*>
        fieldValidation (fooDF @t @m) <*>
        fieldValidation (fooAF @t @m) <*>
        fieldValidation (fooBF @t @m) <*>
        fieldValidation (fooCF @t @m) 

    fooW =
      workflowWidget
        [ WorkflowStep "Workflow - Page 1" fooNF []
        , WorkflowStep "Workflow - Page 2" fooDF []
        , WorkflowStep "Workflow - Page 3" fooAF []
        , WorkflowStep "Workflow - Page 4" fooBF []
        , WorkflowStep "Workflow - Page 5" fooCF []
        ]
        workflowWidgetConfig
  in
    Field foo fooU (idApp "-foo") fooV fooW

{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Dom.Validation where

import Control.Monad (void, join)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Functor.Classes
import Data.Functor.Compose (Compose(..))
import Data.List (nub)
import Data.Semigroup (Semigroup(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Generic1)

import Control.Monad.Trans (liftIO, lift)

import Control.Lens

import Reflex.Dom.Core

import Data.Validation
import Data.Aeson (ToJSON(..), FromJSON(..), ToJSON1(..), FromJSON1(..))

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Bootstrap

import Data.Dependent.Map (GCompare)
import Reflex.Dom.Storage.Base
import Reflex.Dom.Storage.Class
import Data.GADT.Aeson

class NFunctor f where
  nmap :: (forall x. g x -> h x) -> f g -> f h

instance (Functor k, NFunctor f) => NFunctor (Compose k f) where
  nmap f = Compose . fmap (nmap f) . getCompose

data Id = Id {
    _idParent :: Maybe Id
  , _idTag :: Text
  }
  deriving (Eq, Ord, Show, Read)

makeLenses ''Id

idApp :: Text -> Id -> Id
idApp t i = Id (Just i) t

idToText :: Id -> Text
idToText (Id mi t) = maybe ""  idToText mi <> t

matchOrDescendant :: Id -> Id -> Bool
matchOrDescendant i1 i2 =
  i1 == i2 ||
  maybe False (matchOrDescendant i1) (view idParent i2)

data WithId a = WithId { _wiId :: Id, _wiValue :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

makeLenses ''WithId

hasMatchingErrors :: Id -> [WithId e] -> Bool
hasMatchingErrors i = any ((== i) . view wiId)

class HasErrorMessage e where
  errorMessage :: e -> Text

newtype Wrap a f = Wrap {unWrap :: f a }
  deriving (Generic)

makeWrapped ''Wrap

instance (Eq1 f, Eq a) => Eq (Wrap a f) where
  Wrap x == Wrap y = liftEq (==) x y

instance (Ord1 f, Ord a) => Ord (Wrap a f) where
  compare (Wrap x) (Wrap y) = liftCompare compare x y

instance (Show1 f, Show a) => Show (Wrap a f) where
  showsPrec n (Wrap x) = liftShowsPrec showsPrec showList n x

instance (Read1 f, Read a) => Read (Wrap a f) where
  readsPrec = fmap (fmap (first Wrap)) <$> liftReadsPrec readsPrec readList

instance (ToJSON1 f, ToJSON a) => ToJSON (Wrap a f) where
  toJSON (Wrap x) = liftToJSON toJSON toJSONList x

instance (FromJSON1 f, FromJSON a) => FromJSON (Wrap a f) where
  parseJSON = fmap Wrap <$> liftParseJSON parseJSON parseJSONList

class Semigroup1 f where
  sappend1 :: f a -> f a -> f a

class Semigroup1 f => Monoid1 f where
  mempty1 :: f a
  mappend1 :: f a -> f a -> f a
  mappend1 = sappend1

instance Semigroup1 Maybe where
  sappend1 Nothing x = x
  sappend1 x Nothing = x
  sappend1 (Just _) (Just y) = Just y

instance Monoid1 Maybe where
  mempty1 = Nothing

instance Semigroup1 f => Semigroup (Wrap a f) where
  Wrap x <> Wrap y = Wrap (sappend1 x y)

instance Monoid1 f => Monoid (Wrap a f) where
  mempty = Wrap mempty1
  mappend = (<>)

instance NFunctor (Wrap a) where
  nmap f (Wrap g) = Wrap (f g)

type ValidationFn e f f' =
  Id -> f Maybe -> Validation (NonEmpty (WithId e)) (f' Identity)

data ValidationWidgetOutput t e f u =
  ValidationWidgetOutput {
    _vwoFailures :: Dynamic t [WithId e]
  , _vwoSuccesses :: Event t (Endo (f Maybe))
  , _vwoUI :: Event t (Endo u)
  }

makeLenses ''ValidationWidgetOutput

instance Reflex t => Semigroup (ValidationWidgetOutput t e f u) where
  ValidationWidgetOutput f1 s1 u1 <> ValidationWidgetOutput f2 s2 u2 =
    ValidationWidgetOutput (f1 <> f2) (s1 <> s2) (u1 <> u2)

instance Reflex t => Monoid (ValidationWidgetOutput t e f u) where
  mempty = ValidationWidgetOutput mempty mempty mempty
  mappend = (<>)

-- TODO change ValidationWidget return type

type ValidationWidget t m e f u =
  Id -> Dynamic t (f Maybe) -> Dynamic t u -> Dynamic t [WithId e] -> m (ValidationWidgetOutput t e f u)

data Field t m e f f' u u' where
  Field :: NFunctor f'
        => (forall g. Functor g => Lens' (f g) (f' g))
        -> Lens' u u'
        -> (Id -> Id)
        -> ValidationFn e f' f'
        -> ValidationWidget t m e f' u'
        -> Field t m e f f' u u'

fieldId :: Field t m e f f' u u' -> Id -> Id
fieldId (Field _ _ fi _ _) i = fi i

fieldValidation' :: (forall g. Lens' (f g) (f' g)) -> (Id -> Id) -> ValidationFn e f' f' -> ValidationFn e f f'
fieldValidation' l fi v i mf =
  v (fi i) (view l mf)

fieldValidation :: Field t m e f f' u u' -> ValidationFn e f f'
fieldValidation f@(Field l _ fi v _) i mf
  = v (fi i) (view l mf)

fieldWidget :: MonadWidget t m => Field t m e f f' u u' -> ValidationWidget t m e f u
fieldWidget f@(Field l lu fi _ w) i dv du de = do
  let
    i' = fi i
  ValidationWidgetOutput d e' u' <- w i' (view l <$> dv) (view lu <$> du) $ filter (matchOrDescendant i' . view wiId) <$> de
  pure $ ValidationWidgetOutput d (Endo . over l . appEndo <$> e') (Endo . over lu . appEndo <$> u')

unwrapV :: Wrap a Identity -> a
unwrapV = runIdentity . unWrap

required :: HasNotSpecified e => ValidationFn e (Wrap a) (Wrap a)
required i (Wrap m) =
  maybe (Failure . pure . WithId i $ _NotSpecified # ()) (Success . Wrap . Identity) m

class HasNotSpecified e where
  _NotSpecified :: Prism' e ()

-- this puts a potential validation button at the bottom, which might not be what we want in all cases
wrapUp :: (MonadWidget t m, Eq e)
       => Field t m e f f u u
       -> f Maybe
       -> u
       -> (Dynamic t (f Maybe) -> m (Event t (f Maybe)))
       -> m (Event t (f Identity))
wrapUp f ini iniU v = mdo
  let i = Id Nothing "top"

  dcr <- foldDyn ($) ini $ fmap appEndo eFn
  du <- foldDyn ($) iniU $ fmap appEndo eU
  ValidationWidgetOutput de eFn eU <- fieldWidget f i dcr du $
    (\x y -> nub $ x ++ y) <$> des <*> de
  eV <- v dcr
  let (eFailure, eSuccess) = fanEither $ toEither . fieldValidation f i <$> eV
  -- TODO printing these failures would be interesting
  des :: Dynamic t [WithId e] <- holdDyn [] . leftmost $
    [ NonEmpty.toList <$> eFailure
    , [] <$ eSuccess
    ]

  pure eSuccess

wrapUpStorage :: (MonadWidget t m, Eq e, GKey k, GCompare k, ToJSONTag k Identity, FromJSONTag k Identity, NFunctor f)
              => Field t m e f f u u
              -> k (f Maybe)
              -> f Maybe
              -> k u
              -> u
              -> (Dynamic t (f Maybe) -> m (Event t (f Maybe)))
              -> m (Event t (f Identity))
wrapUpStorage f k ini kU iniU v = runStorageT LocalStorage $ do
  initializeTag k ini
  dTag <- askStorageTagDef k ini
  iTag <- sample . current $ dTag

  initializeTag kU iniU
  duTag <- askStorageTagDef kU iniU
  iuTag <- sample . current $ duTag

  (eM, eI, eU) <- lift $ mdo
    let i = Id Nothing "top"

    dcr <- foldDyn ($) iTag . leftmost $ [fmap appEndo eFn, const <$> updated dTag]
    dU <- foldDyn ($) iuTag . leftmost $ [fmap appEndo eU, const <$> updated duTag]

    ValidationWidgetOutput de eFn eU <- fieldWidget f i dcr dU $
      (\x y -> nub $ x ++ y) <$> des <*> de
    eV <- v dcr
    let (eFailure, eSuccess) = fanEither $ toEither . fieldValidation f i <$> eV
    -- TODO printing these failures would be interesting
    des :: Dynamic t [WithId e] <- holdDyn [] . leftmost $
      [ NonEmpty.toList <$> eFailure
      , [] <$ eSuccess
      ]

    pure (eFn, eSuccess, eU)

  tellStorageInsert k $ flip appEndo <$> current dTag <@> eM
  -- tellStorageInsert k $ nmap (Just . runIdentity) <$> eI
  tellStorageInsert kU $ flip appEndo <$> current duTag <@> eU

  pure eI




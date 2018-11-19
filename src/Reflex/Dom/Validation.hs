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

import Control.Monad.Trans (liftIO)

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

instance Semigroup1 f => Semigroup (Wrap a f) where
  Wrap x <> Wrap y = Wrap (sappend1 x y)

instance Monoid1 f => Monoid (Wrap a f) where
  mempty = Wrap mempty1
  mappend = (<>)

instance NFunctor (Wrap a) where
  nmap f (Wrap g) = Wrap (f g)

type ValidationFn e f f' =
  Id -> f Maybe -> Validation (NonEmpty (WithId e)) (f' Identity)

data ValidationWidgetOutput t e f =
  ValidationWidgetOutput {
    _vwoFailures :: Dynamic t [WithId e]
  , _vwoSuccesses :: Event t (Endo (f Maybe))
  }

makeLenses ''ValidationWidgetOutput

instance Reflex t => Semigroup (ValidationWidgetOutput t e f) where
  ValidationWidgetOutput f1 s1 <> ValidationWidgetOutput f2 s2 =
    ValidationWidgetOutput (f1 <> f2) (s1 <> s2)

instance Reflex t => Monoid (ValidationWidgetOutput t e f) where
  mempty = ValidationWidgetOutput mempty mempty
  mappend = (<>)

-- TODO change ValidationWidget return type

type ValidationWidget t m e f =
  Id -> Dynamic t (f Maybe) -> Dynamic t [WithId e] -> m (ValidationWidgetOutput t e f)

data Field t m e f f' where
  Field :: NFunctor f'
        => (forall g. Functor g => Lens' (f g) (f' g))
        -> (Id -> Id)
        -> ValidationFn e f' f'
        -> ValidationWidget t m e f'
        -> Field t m e f f'

fieldId :: Field t m e f f' -> Id -> Id
fieldId (Field _ fi _ _) i = fi i

fieldValidation :: Field t m e f f' -> ValidationFn e f f'
fieldValidation f@(Field l fi v _) i mf
  = v (fi i) (view l mf)

fieldWidget :: MonadWidget t m => Field t m e f f' -> ValidationWidget t m e f
fieldWidget f@(Field l fi _ w) i dv de = do
  let
    i' = fi i
  ValidationWidgetOutput d e' <- w i' (view l <$> dv) $ filter (matchOrDescendant i' . view wiId) <$> de
  pure . ValidationWidgetOutput d $ Endo . over l . appEndo <$> e'

unwrapV :: Wrap a Identity -> a
unwrapV = runIdentity . unWrap

required :: HasNotSpecified e => ValidationFn e (Wrap a) (Wrap a)
required i (Wrap m) =
  maybe (Failure . pure . WithId i $ _NotSpecified # ()) (Success . Wrap . Identity) m

class HasNotSpecified e where
  _NotSpecified :: Prism' e ()

-- this puts a potential validation button at the bottom, which might not be what we want in all cases
wrapUp :: (MonadWidget t m, Eq e)
       => Field t m e f f
       -> f Maybe
       -> (Dynamic t (f Maybe) -> m (Event t (f Maybe)))
       -> m (Event t (f Identity))
wrapUp f ini v = mdo
  let i = Id Nothing "top"

  dcr <- foldDyn ($) ini $ fmap appEndo eFn
  ValidationWidgetOutput de eFn <- fieldWidget f i dcr $
    (\x y -> nub $ x ++ y) <$> des <*> de
  eV <- v dcr
  let (eFailure, eSuccess) = fanEither $ toEither . fieldValidation f i <$> eV
  -- TODO printing these failures would be interesting
  des :: Dynamic t [WithId e] <- holdDyn [] . leftmost $
    [ NonEmpty.toList <$> eFailure
    , [] <$ eSuccess
    ]

  pure eSuccess


{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.Dom.Validation.Wrap where

import Data.Bifunctor (first)
import Data.Functor.Classes
import Data.Functor.Identity (Identity(..))

import GHC.Generics (Generic)

import Control.Lens

import Data.Aeson

import Reflex.Dom.Validation.Classes

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

instance Semigroup1 f => Semigroup (Wrap a f) where
  Wrap x <> Wrap y = Wrap (sappend1 x y)

instance Monoid1 f => Monoid (Wrap a f) where
  mempty = Wrap mempty1
  mappend = (<>)

instance NFunctor (Wrap a) where
  nmap f (Wrap g) = Wrap (f g)

unwrapV :: Wrap a Identity -> a
unwrapV = runIdentity . unWrap

mapWrap :: Functor f => (a -> b) -> Wrap a f -> Wrap b f
mapWrap f (Wrap x) = Wrap (fmap f x)

pureWrap :: Applicative f => a -> Wrap a f
pureWrap = Wrap . pure

apWrap :: Applicative f => Wrap (a -> b) f -> Wrap a f -> Wrap b f
apWrap (Wrap f) (Wrap x) = Wrap (f <*> x)

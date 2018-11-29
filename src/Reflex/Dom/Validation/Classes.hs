{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RankNTypes #-}
module Reflex.Dom.Validation.Classes where

import Data.Functor.Compose (Compose(..))

class NFunctor f where
  nmap :: (forall x. g x -> h x) -> f g -> f h

instance (Functor k, NFunctor f) => NFunctor (Compose k f) where
  nmap f = Compose . fmap (nmap f) . getCompose

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

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
import Data.Bool (bool)
import Data.Functor.Compose (Compose(..))
import Data.Semigroup (Semigroup(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)

import Control.Monad.Trans (liftIO)

import Control.Lens

import Reflex.Dom.Core

import Data.Validation
import Data.Aeson (ToJSON, FromJSON)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Bootstrap

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

class HasErrorMessage e where
  errorMessage :: e -> Text

newtype Wrap a f = Wrap {unWrap :: f a }
  deriving (Eq, Ord, Show, Read, Generic)

makeWrapped ''Wrap

instance ToJSON a => ToJSON (Wrap a Maybe) where
instance FromJSON a => FromJSON (Wrap a Maybe) where

type ValidationFn e f f' =
  Id -> f Maybe -> Validation (NonEmpty (WithId e)) (f' Identity)

type ValidationWidget t m e f =
  Id -> Dynamic t (f Maybe) -> Dynamic t [WithId e] -> m (Event t (Endo (f Maybe)))

data Field t m e f f' where
  Field :: (forall g. Functor g => Lens' (f g) (f' g))
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
  e' <- w i' (view l <$> dv) $ filter (matchOrDescendant i' . view wiId) <$> de
  pure $ Endo . over l . appEndo <$> e'

unwrapV :: Wrap a Identity -> a
unwrapV = runIdentity . unWrap

required :: HasNotSpecified e => ValidationFn e (Wrap a) (Wrap a)
required i (Wrap m) =
  maybe (Failure . pure . WithId i $ _NotSpecified # ()) (Success . Wrap . Identity) m

errorsForId :: (MonadWidget t m, HasErrorMessage e)
            => Id -> Dynamic t [WithId e] -> m ()
errorsForId i des =
  let
    dErrors = fmap (view wiValue) . ffilter ((== i) . view wiId) <$> des
  in
    divClass "invalid-feedback" . void . simpleList dErrors $
      dynText . fmap errorMessage

class HasNotSpecified e where
  _NotSpecified :: Prism' e ()



-- this puts a potential validation button at the bottom, which might not be what we want in all cases
wrapUp :: MonadWidget t m
       => Field t m e f f
       -> f Maybe
       -> (Dynamic t (f Maybe) -> m (Event t (f Maybe)))
       -> m (Event t (f Identity))
wrapUp f ini v = mdo
  let i = Id Nothing "top"

  dcr <- foldDyn ($) ini $ fmap appEndo eFn
  eFn <- fieldWidget f i dcr des
  eV <- v dcr
  let (eFailure, eSuccess) = fanEither $ toEither . fieldValidation f i <$> eV
  -- TODO printing these failures would be interesting
  des :: Dynamic t [WithId e] <- holdDyn [] . leftmost $ [NonEmpty.toList <$> eFailure, [] <$ eSuccess]

  pure eSuccess


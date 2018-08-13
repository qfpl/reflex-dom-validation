{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Reflex.Dom.Validation where

import Control.Monad (void, join)
import Control.Monad.Fix (MonadFix)
import Data.Functor.Identity (Identity(..))
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import Data.Monoid (Endo(..))

import GHC.Generics (Generic)

import Control.Lens

import Control.Monad.Trans (lift)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Data.Text (Text)

import Data.Validation

import Data.Aeson (ToJSON, FromJSON)

import Reflex.Dom.Core

newtype Id = Id { unId :: Text }
  deriving (Eq, Ord, Show, Read)

instance Semigroup Id where
  Id x <> Id y = Id (x <> y)

instance IsString Id where
  fromString = Id . fromString

instance Monoid Id where
  mempty = Id mempty
  mappend = (<>)

data WithId a = WithId { _wiId :: Id, _wiValue :: a}

class HasErrorMessage e where
  errorMessage :: e -> Text

class AsNotSpecified e where
  _NotSpecified :: Prism' e ()

required :: AsNotSpecified e
         => r
         -> Id
         -> Maybe a
         -> Validation (NonEmpty (WithId e)) (Identity a)
required _ i Nothing =
  Failure . pure . WithId i $ _NotSpecified # ()
required _ _ (Just x) =
  Success . Identity $ x

optional :: r
         -> Id
         -> Maybe a
         -> Validation (NonEmpty (WithId e)) (Maybe a)
optional _ _ = Success

class NFunctor ty where
  nempty :: ty Maybe
  nmap :: (forall x. f x -> g x) -> ty f -> ty g
  nmap f = runIdentity . ntraverse (Identity . f)
  ntraverse :: Applicative t => (forall x. f x -> t (g x)) -> ty f -> t (ty g)

newtype Wrap a f = Wrap {unWrap :: f a }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON a => ToJSON (Wrap a Maybe) where
instance FromJSON a => FromJSON (Wrap a Maybe) where

instance NFunctor (Wrap a) where
  nempty = Wrap Nothing
  ntraverse f (Wrap x) = Wrap <$> f x

data FieldWidgetConfig t m r e a b =
  FieldWidgetConfig {
    _fwcValidate :: r -> Id -> a -> Validation (NonEmpty (WithId e)) b
  , _fwcRender :: Dynamic t r -> Id -> a -> Event t a -> Dynamic t [e] -> m (Event t a)
  }

newtype FieldWidget' t m r e a b =
  FieldWidget' {
    runFieldWidget :: Dynamic t r -> Id -> a -> Event t a -> Event t () -> m (Dynamic t a, Event t b)
  }

instance (Reflex t, Monad m) => Functor (FieldWidget' t m r e a) where
  fmap f fw = FieldWidget' $ \dr i a ea ev -> do
    (d, e) <- runFieldWidget fw dr i a ea ev
    pure (d, fmap f e)

instance (Reflex t, Monad m) => FunctorMaybe (FieldWidget' t m r e a) where
  fmapMaybe f fw = FieldWidget' $ \dr i a ea ev -> do
    (d, e) <- runFieldWidget fw dr i a ea ev
    pure (d, fmapMaybe f e)

type FieldWidget t m r e ty = FieldWidget' t m r e (ty Maybe) (ty Identity)

mkFieldWidget' :: MonadWidget t m
               => FieldWidgetConfig t m r e (Maybe a) (Identity a)
               -> FieldWidget' t m r e (Maybe a) (Identity a)
mkFieldWidget' (FieldWidgetConfig v r) = FieldWidget' $ \dr i ia ea ev -> mdo
  dState <- holdDyn ia $ leftmost [Just . runIdentity <$> eSuccesses, Nothing <$ eErrors]
  dErrors <- holdDyn [] $ leftmost [fmap _wiValue . NonEmpty.toList <$> eErrors, [] <$ eSuccesses]
  eChange' <- r dr i ia ea dErrors
  let
    eChange = leftmost [eChange', current dState <@ ev]
    (eErrors, eSuccesses) = fanEither $ (\r' -> toEither . v r' i) <$> current dr <@> eChange

  pure (dState, eSuccesses)

wrapFieldWidget :: MonadWidget t m => FieldWidget' t m r e (f a) (g b) -> FieldWidget' t m r e (Wrap a f) (Wrap b g)
wrapFieldWidget w = FieldWidget' $ \dr i ia ea ev -> do
  (da, eb) <- runFieldWidget w dr i (unWrap ia) (unWrap <$> ea) ev
  pure (Wrap <$> da, Wrap <$> eb)

mkFieldWidget :: MonadWidget t m
              => FieldWidgetConfig t m r e (Maybe a) (Identity a)
              -> FieldWidget t m r e (Wrap a)
mkFieldWidget = wrapFieldWidget . mkFieldWidget'

blankFieldWidget :: (Reflex t, Monad m) => FieldWidget t m r e a
blankFieldWidget = FieldWidget' $ \_ _ a _ _ ->
  pure (pure a, never)

data Field t m r e a where
  Field :: NFunctor a'
        => (r -> a Maybe -> r')
        -> (Id -> Id)
        -> Lens' (a Maybe) (a' Maybe)
        -> FieldWidget t m r' e a'
        -> Field t m r e a

nest :: (MonadWidget t m, NFunctor a)
     => ((Field t m r e a -> EventWriterT t (Endo (a Maybe)) m ()) -> EventWriterT t (Endo (a Maybe)) m ())
     -> FieldWidget t m r e a
nest mkN = FieldWidget' $ \dr i ia ea ev -> mdo
  (_, eE) <- runEventWriterT . mkN $ \fp -> case fp of
    Field ctx iFn l w -> do
      let ea' = leftmost [ea, updated dState]
      (dc, _) <- lift $
        runFieldWidget
          w
          (ctx <$> dr <*> dState)
          (iFn i)
          (view l ia)
          (view l <$> ea')
          ev
      tellEvent $ Endo . set l <$> updated dc
  dState <- foldDyn ($) ia $ appEndo <$> eE
  pure (dState, fmapMaybe id $ ntraverse (fmap Identity) <$> updated dState)

data Piece t m r e a s where
  FieldPiece  :: m s
              -> m s
              -> Field t m r e a
              -> Piece t m r e a s
  WidgetPiece :: m s
              -> Piece t m r e a s

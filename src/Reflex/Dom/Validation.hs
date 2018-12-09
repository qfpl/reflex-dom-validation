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
{-# LANGUAGE DataKinds #-}
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
import Control.Monad.Fix (MonadFix)
import Data.Bifunctor (first)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Functor.Classes
import Data.Functor.Compose (Compose(..))
import Data.List (nub)
import Data.Semigroup (Semigroup(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))

import Control.Monad.Ref (MonadRef(..), MonadAtomicRef(..))

import GHC.Generics (Generic, Generic1)

import Control.Monad.Trans (MonadTrans(..), liftIO, lift)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), runReaderT)
import Control.Monad.State (StateT(..), MonadState(..), runStateT, evalStateT, modify)

import Control.Lens hiding (element)

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
import qualified Data.Dependent.Map as DMap

import Reflex.Dom.Storage.Base
import Reflex.Dom.Storage.Class
import Data.GADT.Aeson

import Reflex.Dom.Validation.Classes
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Wrap

-- type ValidationFn e f f' =
--   Id -> f Maybe -> Validation (NonEmpty (WithId e)) (f' Identity)

data ValidationCtx f = 
  ValidationCtx {
    _vcId :: Id
  , _vcInput :: f Maybe
  }

makeLenses ''ValidationCtx

newtype ValidationFn' e f a = 
  ValidationFn' { 
    unValidationFn :: ReaderT (ValidationCtx f) (Validation (NonEmpty (WithId e))) a
  } deriving (Functor, Applicative)

type ValidationFn e f f' = ValidationFn' e f (f' Identity)

toValidationFn :: (Id -> f Maybe -> Validation (NonEmpty (WithId e)) (f' Identity)) -> ValidationFn e f f'
toValidationFn f = ValidationFn' (ReaderT (\(ValidationCtx i v) -> f i v))

runValidationFn :: ValidationFn e f f' -> Id -> f Maybe -> Validation (NonEmpty (WithId e)) (f' Identity)
runValidationFn f i v = runReaderT (unValidationFn f) (ValidationCtx i v)

data ValidationWidgetCtx t e f u =
  ValidationWidgetCtx {
    _vwcId :: Id
  , _vwcValue :: Dynamic t (f Maybe)
  , _vwcUi :: Dynamic t u
  , _vwcErrors :: Dynamic t [WithId e]
  }

makeLenses ''ValidationWidgetCtx

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

-- type ValidationWidget t m e f u =
--   Id -> Dynamic t (f Maybe) -> Dynamic t u -> Dynamic t [WithId e] -> m (ValidationWidgetOutput t e f u)

newtype ValidationWidget t m e f u a =
  ValidationWidget {
    unValidationWidget :: ReaderT (ValidationWidgetCtx t e f u) (StateT (ValidationWidgetOutput t e f u) m) a
  } deriving (Functor, Applicative, Monad, MonadFix, MonadReader (ValidationWidgetCtx t e f u), MonadState (ValidationWidgetOutput t e f u))

liftW :: Monad m => m a -> ValidationWidget t m e f u a
liftW x = ValidationWidget . lift . lift $ x

unliftW :: (Functor m, Reflex t) => (forall x. m x -> m x) -> ValidationWidget t m e f u () -> ValidationWidget t m e f u ()
unliftW f w = toValidationWidget $ \i dv du des ->
  f $ runValidationWidget w i dv du des

instance PerformEvent t m => PerformEvent t (ValidationWidget t m e f u) where
  type Performable (ValidationWidget t m e f u) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = liftW $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = liftW $ performEvent e

instance PostBuild t m => PostBuild t (ValidationWidget t m e f u) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = liftW getPostBuild

instance MonadRef m => MonadRef (ValidationWidget t m e f u) where
  type Ref (ValidationWidget t m e f u) = Ref m
  {-# INLINABLE newRef #-}
  newRef = liftW . newRef
  {-# INLINABLE readRef #-}
  readRef = liftW . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = liftW . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (ValidationWidget t m e f u) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = liftW . atomicModifyRef r

instance MonadSample t m => MonadSample t (ValidationWidget t m e f u) where
  {-# INLINABLE sample #-}
  sample = liftW . sample

instance MonadHold t m => MonadHold t (ValidationWidget t m e f u) where
  {-# INLINABLE hold #-}
  hold v0 v' = liftW $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = liftW $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = liftW $ holdIncremental v0 v'
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = liftW . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = liftW . headE

instance NotReady t m => NotReady t (ValidationWidget t m e f u) where
  notReadyUntil = liftW . notReadyUntil
  notReady = liftW notReady

-- TODO: make this not super dodgy
-- at the moment we have Dynamics and Events in the state
-- and we aren't dealing with them here
--
-- instance (Adjustable t m) => Adjustable t (ValidationWidget t m e f u) where
--   runWithReplace a0 a' = do
--     r <- ask
--     old <- get
--     ((x, new), e) <- liftW $ runWithReplace (flip runStateT old . flip runReaderT r . unValidationWidget $ a0) $ fmap (flip runStateT old . flip runReaderT r . unValidationWidget) a'
--     put new
--     pure (x, fmap fst e)
--   traverseIntMapWithKeyWithAdjust f dm0 dm' = do
--     r <- ask
--     old <- get
--     (i, e) <- liftW $ traverseIntMapWithKeyWithAdjust (\k v -> flip runStateT old . flip runReaderT r . unValidationWidget $ f k v) dm0 dm'
--     traverse_ (modify . (<>) . snd) i
--     pure (fmap fst i, fmap (fmap fst) e)
--   traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
--     r <- ask
--     old <- get
--     (i, e) <- liftW $ traverseDMapWithKeyWithAdjustWithMove (\k v -> fmap (\(x,y) -> Compose (y, x)) . flip runStateT old . flip runReaderT r . unValidationWidget $ f k v) dm0 dm'
--     put . DMap.foldrWithKey (\_ v b -> (b <>) . fst . getCompose $ v) old $ i
--     pure (DMap.map (snd . getCompose) i, mapPatchDMapWithMove (snd . getCompose) <$> e)
-- 
-- instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (ValidationWidget t m e f u) where
--   type DomBuilderSpace (ValidationWidget t m e f u) = DomBuilderSpace m
--   textNode = liftW . textNode
--   element elementTag cfg (ValidationWidget child) = ValidationWidget $ do
--     r <- ask
--     old <- get
--     (el, (a, new)) <- lift $ lift $ element elementTag cfg $ runStateT (runReaderT child r) old
--     put new
--     return (el, a)
--   inputElement = liftW . inputElement
--   textAreaElement = liftW . textAreaElement
--   selectElement cfg (ValidationWidget child) = ValidationWidget $ do
--     r <- ask
--     old <- get
--     (el, (a, new)) <- lift $ lift $ selectElement cfg $ runStateT (runReaderT child r) old
--     put new
--     return (el, a)
--   placeRawElement = liftW . placeRawElement
--   wrapRawElement e = liftW . wrapRawElement e

toValidationWidget :: (Functor m, Reflex t) => (Id -> Dynamic t (f Maybe) -> Dynamic t u -> Dynamic t [WithId e] -> m (a, ValidationWidgetOutput t e f u)) -> ValidationWidget t m e f u a
toValidationWidget f = 
  ValidationWidget . ReaderT $ \(ValidationWidgetCtx i dv du des) -> 
    StateT $ \s ->  do
      fmap (s <>) <$> f i dv du des

toValidationWidget_ :: (Functor m, Reflex t) => (Id -> Dynamic t (f Maybe) -> Dynamic t u -> Dynamic t [WithId e] -> m (ValidationWidgetOutput t e f u)) -> ValidationWidget t m e f u ()
toValidationWidget_ f = toValidationWidget (\i dv du des -> fmap (\x -> ((), x)) (f i dv du des))

runValidationWidget :: Reflex t => ValidationWidget t m e f u a -> Id -> Dynamic t (f Maybe) -> Dynamic t u -> Dynamic t [WithId e] -> m (a, ValidationWidgetOutput t e f u)
runValidationWidget f i dv du des = 
  runStateT (runReaderT (unValidationWidget f) (ValidationWidgetCtx i dv du des)) mempty

runValidationWidget_ :: (Functor m, Reflex t) => ValidationWidget t m e f u a -> Id -> Dynamic t (f Maybe) -> Dynamic t u -> Dynamic t [WithId e] -> m (ValidationWidgetOutput t e f u)
runValidationWidget_ f i dv du des = fmap snd $ runValidationWidget f i dv du des

data Field t m e f f' u u' where
  Field :: NFunctor f'
        => (forall g. Functor g => Lens' (f g) (f' g))
        -> Lens' u u'
        -> (Id -> Id)
        -> ValidationFn e f' f'
        -> ValidationWidget t m e f' u' ()
        -> Field t m e f f' u u'

fieldId :: Field t m e f f' u u' -> Id -> Id
fieldId (Field _ _ fi _ _) i = fi i

fieldValidation' :: (forall g. Lens' (f g) (f' g)) -> (Id -> Id) -> ValidationFn e f' f' -> ValidationFn e f f'
fieldValidation' l fi v = toValidationFn $ \i mf ->
  runValidationFn v (fi i) (view l mf)

fieldValidation :: Field t m e f f' u u' -> ValidationFn e f f'
fieldValidation f@(Field l _ fi v _) = toValidationFn $ \i mf ->
  runValidationFn v (fi i) (view l mf)

fieldWidget :: MonadWidget t m => Field t m e f f' u u' -> ValidationWidget t m e f u ()
fieldWidget f@(Field l lu fi _ w) = toValidationWidget_ $ \i dv du de -> do
  let
    i' = fi i
  ValidationWidgetOutput d e' u' <- runValidationWidget_ w i' (view l <$> dv) (view lu <$> du) $ filter (matchOrDescendant i' . view wiId) <$> de
  pure (ValidationWidgetOutput d (Endo . over l . appEndo <$> e') (Endo . over lu . appEndo <$> u'))

optional :: ValidationFn e (Wrap (Maybe a)) (Wrap (Maybe a))
optional = toValidationFn $ \_ ->
  Success . Wrap . Identity . join . unWrap

-- TODO required but blankable
requiredMaybe :: HasNotSpecified e => ValidationFn e (Wrap (Maybe a)) (Wrap (Maybe a))
requiredMaybe = toValidationFn $ \i ->
  maybe 
    (Failure . pure . WithId i $ _NotSpecified # ()) 
    (Success . Wrap . Identity . Just) . 
  join . 
  unWrap

required :: HasNotSpecified e => ValidationFn e (Wrap a) (Wrap a)
required = toValidationFn $ \i (Wrap m) ->
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
  ValidationWidgetOutput de eFn eU <- runValidationWidget_ (fieldWidget f) i dcr du $
    (\x y -> nub $ x ++ y) <$> des <*> de
  eV <- v dcr
  let (eFailure, eSuccess) = fanEither $ toEither . runValidationFn (fieldValidation f) i <$> eV
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
              -> k [WithId e]
              -> (Dynamic t (f Maybe) -> m (Event t (f Maybe)))
              -> m (Event t (f Identity))
wrapUpStorage f k ini kU iniU kE v = runStorageT LocalStorage $ do
  initializeTag k ini
  dTag <- askStorageTagDef k ini
  iTag <- sample . current $ dTag

  initializeTag kU iniU
  duTag <- askStorageTagDef kU iniU
  iuTag <- sample . current $ duTag

  initializeTag kE []
  deTag <- askStorageTagDef kE []
  ieTag <- sample . current $ deTag

  (eM, eI, eU, eE) <- lift $ mdo
    let i = Id Nothing "top"

    dcr <- foldDyn ($) iTag . leftmost $ [fmap appEndo eFn, const <$> updated dTag]
    dU <- foldDyn ($) iuTag . leftmost $ [fmap appEndo eU, const <$> updated duTag]

    ValidationWidgetOutput de eFn eU <- runValidationWidget_ (fieldWidget f) i dcr dU $
      (\x y -> nub $ x ++ y) <$> des <*> de
    eV <- v dcr
    let (eFailure, eSuccess) = fanEither $ toEither . runValidationFn (fieldValidation f) i <$> eV
    let eE = leftmost [NonEmpty.toList <$> eFailure, [] <$ eSuccess]
    des :: Dynamic t [WithId e] <- holdDyn ieTag . leftmost $ [eE , updated deTag]

    pure (eFn, eSuccess, eU, eE)

  tellStorageInsert k $ flip appEndo <$> current dTag <@> eM
  -- tellStorageInsert k $ nmap (Just . runIdentity) <$> eI
  tellStorageInsert kU $ flip appEndo <$> current duTag <@> eU
  tellStorageInsert kE eE

  pure eI




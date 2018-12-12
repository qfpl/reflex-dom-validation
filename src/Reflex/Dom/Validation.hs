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
import Data.Foldable (traverse_, fold)
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
import Data.Functor.Misc

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

data ValidationCtx v f =
  ValidationCtx {
    _vcId :: Id
  , _vcContext :: v
  , _vcInput :: f Maybe
  }

makeLenses ''ValidationCtx

newtype ValidationFn' e v f a =
  ValidationFn' {
    unValidationFn :: ReaderT (ValidationCtx v f) (Validation (NonEmpty (WithId e))) a
  } deriving (Functor, Applicative)

type ValidationFn e v f f' = ValidationFn' e v f (f' Identity)

toValidationFn :: (Id -> v -> f Maybe -> Validation (NonEmpty (WithId e)) (f' Identity))
               -> ValidationFn e v f f'
toValidationFn f =
  ValidationFn' (ReaderT (\(ValidationCtx i c v) -> f i c v))

runValidationFn :: ValidationFn e v f f'
                -> Id
                -> v
                -> f Maybe
                -> Validation (NonEmpty (WithId e)) (f' Identity)
runValidationFn f i ctx v =
  runReaderT (unValidationFn f) (ValidationCtx i ctx v)

data ValidationWidgetCtx t e f u v =
  ValidationWidgetCtx {
    _vwcId :: Id
  , _vwcValue :: Dynamic t (f Maybe)
  , _vwcUi :: Dynamic t u
  , _vwcCtx :: Dynamic t v
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

newtype ValidationWidget t e f u v m a =
  ValidationWidget {
    unValidationWidget :: ReaderT (ValidationWidgetCtx t e f u v) (StateT (ValidationWidgetOutput t e f u) m) a
  } deriving (Functor, Applicative, Monad, MonadFix, MonadReader (ValidationWidgetCtx t e f u v), MonadState (ValidationWidgetOutput t e f u))

toValidationWidget :: (Functor m, Reflex t)
                   => (Id -> Dynamic t (f Maybe) -> Dynamic t u -> Dynamic t v -> Dynamic t [WithId e] -> m (a, ValidationWidgetOutput t e f u))
                   -> ValidationWidget t e f u v m a
toValidationWidget f =
  ValidationWidget . ReaderT $ \(ValidationWidgetCtx i dv du dc des) ->
    StateT $ \s ->
      fmap (s <>) <$> f i dv du dc des

toValidationWidget_ :: (Functor m, Reflex t)
                    => (Id -> Dynamic t (f Maybe) -> Dynamic t u -> Dynamic t v -> Dynamic t [WithId e] -> m (ValidationWidgetOutput t e f u))
                    -> ValidationWidget t e f u v m ()
toValidationWidget_ f =
  toValidationWidget (\i dv du dc des -> fmap (\x -> ((), x)) (f i dv du dc des))

runValidationWidget' :: ValidationWidget t e f u v m a
                     -> ValidationWidgetCtx t e f u v
                     -> ValidationWidgetOutput t e f u
                     -> m (a, ValidationWidgetOutput t e f u)
runValidationWidget' w r =
  runStateT (runReaderT (unValidationWidget w) r)

runValidationWidget :: Reflex t
                    => ValidationWidget t e f u v m a
                    -> Id
                    -> Dynamic t (f Maybe)
                    -> Dynamic t u
                    -> Dynamic t v
                    -> Dynamic t [WithId e]
                    -> m (a, ValidationWidgetOutput t e f u)
runValidationWidget w i dv du dc des =
  runValidationWidget' w (ValidationWidgetCtx i dv du dc des) mempty

runValidationWidget_ :: (Functor m, Reflex t)
                     => ValidationWidget t e f u v m a
                     -> Id
                     -> Dynamic t (f Maybe)
                     -> Dynamic t u
                     -> Dynamic t v
                     -> Dynamic t [WithId e]
                     -> m (ValidationWidgetOutput t e f u)
runValidationWidget_ f i dv du dc des =
  snd <$> runValidationWidget f i dv du dc des

instance MonadTrans (ValidationWidget t e f u v) where
  lift = ValidationWidget . lift . lift

instance PerformEvent t m => PerformEvent t (ValidationWidget t e f u v m) where
  type Performable (ValidationWidget t e f u v m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (ValidationWidget t e f u v m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance MonadRef m => MonadRef (ValidationWidget t e f u v m) where
  type Ref (ValidationWidget t e f u v m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (ValidationWidget t e f u v m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadSample t m => MonadSample t (ValidationWidget t e f u v m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (ValidationWidget t e f u v m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE

instance NotReady t m => NotReady t (ValidationWidget t e f u v m) where
  notReadyUntil = lift . notReadyUntil
  notReady = lift notReady

instance (MonadHold t m, Adjustable t m) => Adjustable t (ValidationWidget t e f u v m) where
  runWithReplace a0 a' = do
    r <- ask
    old <- get
    (x, e) <- lift $ runWithReplace (runValidationWidget' a0 r old) $
      fmap (\w' -> runValidationWidget' w' r old) a'

    let
      x' = snd x
      e' = snd <$> e
    dE <- holdDyn (_vwoFailures x') (_vwoFailures <$> e')
    eF <- switchHoldPromptOnly (_vwoSuccesses x') (_vwoSuccesses <$> e')
    eU <- switchHoldPromptOnly (_vwoUI x') (_vwoUI <$> e')
    put $ ValidationWidgetOutput (join dE) eF eU

    pure (fst x, fmap fst e)

  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    r <- ask
    old <- get
    (i, e) <- lift $ traverseIntMapWithKeyWithAdjust (\k v -> runValidationWidget' (f k v) r old) dm0 dm'

    -- TODO double check all of the handling of the state here
    -- particularly the parts that deal with the Dynamic
    let
      mVwo = fmap snd i
      emVwo = fmap (fmap snd) e
      switchMe = switchHoldPromptOnlyIncremental mergeIntIncremental coincidencePatchIntMap
      distributeIntOverDynPure =  fmap dmapToIntMap . distributeDMapOverDynPure . intMapWithFunctorToDMap
      joinDynThroughInt = (distributeIntOverDynPure =<<)

    dE <- incrementalToDynamic <$> holdIncremental (fmap _vwoFailures mVwo) (fmap (fmap _vwoFailures) emVwo)
    eF <- switchMe (fmap _vwoSuccesses mVwo) (fmap (fmap _vwoSuccesses) emVwo)
    eU <- switchMe (fmap _vwoUI mVwo) (fmap (fmap _vwoUI) emVwo)

    put $ ValidationWidgetOutput (fold <$> joinDynThroughInt dE) (fold <$> eF) (fold <$> eU)

    pure (fmap fst i, fmap (fmap fst) e)

  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    r <- ask
    old <- get
    (i, e) <- lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> fmap (\(x,y) -> Compose (y, x)) . runValidationWidget' (f k v) r $ old) dm0 dm'

    -- TODO: make this not super dodgy
    -- at the moment we have Dynamics and Events in the state
    -- and we aren't dealing with them here
    put . DMap.foldrWithKey (\_ v b -> (b <>) . fst . getCompose $ v) old $ i

    -- let
      -- mVwo = DMap.map (Const . fst . getCompose) i
      -- emVwo = mapPatchDMapWithMove (Const . fst . getCompose) <$> e
      -- switchMe = switchHoldPromptOnlyIncremental mergeIntIncremental coincidencePatchIntMap

    -- dE <- _
    -- eF <- _
    -- eU <- _
    -- put $ ValidationWidgetOutput dE eF eU

    pure (DMap.map (snd . getCompose) i, mapPatchDMapWithMove (snd . getCompose) <$> e)

instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (ValidationWidget t e f u v m) where
  type DomBuilderSpace (ValidationWidget t e f u v m) = DomBuilderSpace m
  textNode = liftTextNode
  element elementTag cfg (ValidationWidget child) = ValidationWidget $ do
    r <- ask
    old <- get
    (l, (a, new)) <- lift $ lift $ element elementTag cfg $ runStateT (runReaderT child r) old
    put new
    return (l, a)
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (ValidationWidget child) = ValidationWidget $ do
    r <- ask
    old <- get
    (l, (a, new)) <- lift $ lift $ selectElement cfg $ runStateT (runReaderT child r) old
    put new
    return (l, a)
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

data Field t m e f f' u u' v v' where
  Field :: NFunctor f'
        => (forall g. Functor g => Lens' (f g) (f' g))
        -> Lens' u u'
        -> (f Maybe -> v -> v')
        -> (Id -> Id)
        -> ValidationFn e v' f' f'
        -> ValidationWidget t e f' u' v' m ()
        -> Field t m e f f' u u' v v'

fieldId :: Field t m e f f' u u' v v' -> Id -> Id
fieldId (Field _ _ _ fi _ _) i = fi i

fieldValidation :: Field t m e f f' u u' v v' -> ValidationFn e v f f'
fieldValidation f@(Field l _ lc fi v _) = toValidationFn $ \i c f ->
  runValidationFn v (fi i) (lc f c) (view l f)

fieldWidget :: MonadWidget t m => Field t m e f f' u u' v v' -> ValidationWidget t e f u v m ()
fieldWidget f@(Field l lu lc fi _ w) = toValidationWidget_ $ \i dv du dc de -> do
  let
    i' = fi i
  ValidationWidgetOutput d e' u' <- runValidationWidget_ w i' (view l <$> dv) (view lu <$> du) (lc <$> dv <*> dc) $ filter (matchOrDescendant i' . view wiId) <$> de
  pure (ValidationWidgetOutput d (Endo . over l . appEndo <$> e') (Endo . over lu . appEndo <$> u'))

optional :: ValidationFn e v (Wrap (Maybe a)) (Wrap (Maybe a))
optional = toValidationFn $ \_ _ ->
  Success . Wrap . Identity . join . unWrap

requiredMaybe :: HasNotSpecified e => ValidationFn e v (Wrap (Maybe a)) (Wrap (Maybe a))
requiredMaybe = toValidationFn $ \i _ ->
  maybe
    (Failure . pure . WithId i $ _NotSpecified # ())
    (Success . Wrap . Identity . Just) .
  join .
  unWrap

required :: HasNotSpecified e => ValidationFn e v (Wrap a) (Wrap a)
required = toValidationFn $ \i _ (Wrap m) ->
  maybe (Failure . pure . WithId i $ _NotSpecified # ()) (Success . Wrap . Identity) m

class HasNotSpecified e where
  _NotSpecified :: Prism' e ()

-- this puts a potential validation button at the bottom, which might not be what we want in all cases
wrapUp :: (MonadWidget t m, Eq e)
       => Field t m e f f u u v v
       -> f Maybe
       -> u
       -> v
       -> (Dynamic t (f Maybe) -> m (Event t (f Maybe)))
       -> m (Event t (f Identity))
wrapUp f ini iniU iniV v = mdo
  let i = Id Nothing "top"

  dcr <- foldDyn ($) ini $ fmap appEndo eFn
  du <- foldDyn ($) iniU $ fmap appEndo eU
  ValidationWidgetOutput de eFn eU <- runValidationWidget_ (fieldWidget f) i dcr du (pure iniV) $
    (\x y -> nub $ x ++ y) <$> des <*> de
  eV <- v dcr
  let (eFailure, eSuccess) = fanEither $ toEither . runValidationFn (fieldValidation f) i iniV <$> eV
  -- TODO printing these failures would be interesting
  des :: Dynamic t [WithId e] <- holdDyn [] . leftmost $
    [ NonEmpty.toList <$> eFailure
    , [] <$ eSuccess
    ]

  pure eSuccess

wrapUpStorage :: (MonadWidget t m, Eq e, GKey k, GCompare k, ToJSONTag k Identity, FromJSONTag k Identity, NFunctor f)
              => Field t m e f f u u v v
              -> k (f Maybe)
              -> f Maybe
              -> k u
              -> u
              -> v
              -> k [WithId e]
              -> (Dynamic t (f Maybe) -> m (Event t (f Maybe)))
              -> m (Event t (f Identity))
wrapUpStorage f k ini kU iniU iniV kE v = runStorageT LocalStorage $ do
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

    ValidationWidgetOutput de eFn eU <- runValidationWidget_ (fieldWidget f) i dcr dU (pure iniV) $
      (\x y -> nub $ x ++ y) <$> des <*> de
    eV <- v dcr
    let (eFailure, eSuccess) = fanEither $ toEither . runValidationFn (fieldValidation f) i iniV <$> eV
    let eE = leftmost [NonEmpty.toList <$> eFailure, [] <$ eSuccess]
    des :: Dynamic t [WithId e] <- holdDyn ieTag . leftmost $ [eE , updated deTag]

    pure (eFn, eSuccess, eU, eE)

  tellStorageInsert k $ flip appEndo <$> current dTag <@> eM
  -- tellStorageInsert k $ nmap (Just . runIdentity) <$> eI
  tellStorageInsert kU $ flip appEndo <$> current duTag <@> eU
  tellStorageInsert kE eE

  pure eI


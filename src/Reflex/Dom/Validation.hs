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
  deriving (Eq, Ord, Show, Read)

class HasErrorMessage e where
  errorMessage :: e -> Text

class AsNotSpecified e where
  _NotSpecified :: Prism' e ()

required :: AsNotSpecified e
         => Id
         -> r
         -> Maybe a
         -> Validation (NonEmpty (WithId e)) a
required i _ Nothing =
  Failure . pure . WithId i $ _NotSpecified # ()
required _ _ (Just x) =
  Success x

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

newtype FW' t m r e a b =
  FW' {
    unFW' :: Id -> Dynamic t r -> Dynamic t a -> Dynamic t b -> Event t () -> m (Event t (Endo a), Event t (Endo b))
  }

newtype FW t m r e ty =
  FW {
    unFW :: FW' t m r e (ty Maybe) (ty (Validation (NonEmpty (WithId e))))
  }

runFW :: (Reflex t, MonadFix m, MonadHold t m)
      => FW t m r e ty
      -> Id
      -> Dynamic t r
      -> Dynamic t (ty Maybe)
      -> ty (Validation (NonEmpty (WithId e)))
      -> Event t ()
      -> m ()
runFW (FW (FW' fn)) i dr da ib ev = mdo
  ia <- sample . current $ da
  da' <- foldDyn ($) ia $ leftmost [appEndo <$> ea, const <$> updated da]
  db' <- foldDyn ($) ib $ appEndo <$> eb
  (ea, eb) <- fn i dr da' db' ev
  pure ()

blankFW :: (Reflex t, Monad m) => FW t m r e ty
blankFW = FW . FW' $ \_ _ _ _ _ -> pure (never, never)

wrapFW :: (Reflex t, Monad m)
       => FW' t m r e (Maybe a) (Validation (NonEmpty (WithId e)) a)
       -> FW t m r e (Wrap a)
wrapFW (FW' fn) = FW . FW' $ \i dr da db ev -> do
  (ea, eb) <- fn i dr (unWrap <$> da) (unWrap <$> db) ev
  pure (Endo . dimap unWrap Wrap . appEndo <$> ea, Endo . dimap unWrap Wrap . appEndo <$> eb)

data F t m r e a where
  F :: (NFunctor a', Eq (a' Maybe), Eq (a' (Validation (NonEmpty (WithId e)))))
    => (Id -> Id)
    -> (r -> a Maybe -> r')
    -> (forall f. Functor f => Lens' (a f) (a' f))
    -> FW t m r' e a'
    -> F t m r e a

data NestW e a =
  NestW {
    _nwPreValidation :: Endo (a Maybe)
  , _nwPostValidation :: Endo (a (Validation (NonEmpty (WithId e))))
  }

instance Semigroup (NestW e a) where
  NestW pre1 post1 <> NestW pre2 post2 =
    NestW (pre1 <> pre2) (post1 <> post2)

instance Monoid (NestW e a) where
  mempty = NestW mempty mempty
  mappend = (<>)

toPre :: Endo (a Maybe) -> NestW e a
toPre e = NestW e mempty

toPost :: Endo (a (Validation (NonEmpty (WithId e)))) -> NestW e a
toPost = NestW mempty

nestF :: (MonadWidget t m, NFunctor a)
      => ((F t m r e a -> EventWriterT t (NestW e a) m ()) -> EventWriterT t (NestW e a) m ())
      -> FW t m r e a
nestF mkN = FW . FW' $ \i dr da db ev -> mdo
  (_, eE) <- runEventWriterT . mkN $ \fp -> case fp of
    F iFn rFn l (FW (FW' fn)) -> do
      da' <- holdUniqDyn $ view l <$> da
      db' <- holdUniqDyn $ view l <$> db
      (ea, eb) <- lift $
        fn
          (iFn i)
          (rFn <$> dr <*> da)
          da'
          db'
          ev
      tellEvent $ toPre . Endo . over l . appEndo <$> ea
      tellEvent $ toPost . Endo . over l . appEndo <$> eb
  pure (_nwPreValidation <$> eE, _nwPostValidation <$> eE)












data FieldWidgetConfig' t m r e a b =
  FieldWidgetConfig' {
    _fwcValidate :: Id -> r -> a -> b
  , _fwcRender :: Id -> Dynamic t r -> Dynamic t a -> Dynamic t [e] -> Event t () -> m (Event t a)
  }

type FieldWidgetConfig t m r e ty =
  FieldWidgetConfig' t m r e (ty Maybe) (ty (Validation (NonEmpty (WithId e))))

newtype FieldWidget' t m r e a =
  FieldWidget' {
    runFieldWidget :: Id -> Dynamic t r -> Dynamic t a -> Event t () -> m (Event t (a -> a))
  }

type FieldWidget t m r e ty =
  FieldWidget' t m r e (ty Maybe)

-- instance (Reflex t, Monad m) => FunctorMaybe (FieldWidget' t m r e a) where
--   fmapMaybe f fw = FieldWidget' $ \dr i da ev -> do
--     (ea, db) <- runFieldWidget fw dr i da ev
--     pure (ea, fmap (fmap _) db)

-- mkFieldWidget' :: MonadWidget t m
--                => FieldWidgetConfig' t m r e (Maybe a) (Validation (NonEmpty (WithId e)) a)
--                -> FieldWidget' t m r e (Maybe a) (Validation (NonEmpty (WithId e)) a)
-- mkFieldWidget' (FieldWidgetConfig' v r) = FieldWidget' $ \i dr da ev -> mdo
--   dErrors <- holdDyn [] $ leftmost [fmap _wiValue . NonEmpty.toList <$> eErrors, [] <$ eSuccesses]
--   eChange' <- r i dr da dErrors ev
--   let
--     eChange = leftmost [eChange', current da <@ ev]
--     eChecked = (\r' -> v i r') <$> current dr <@> eChange
--     (eErrors, eSuccesses) = fanEither $ toEither <$> eChecked

--   pure (eChange, eChecked)

mkFieldWidget' :: (MonadWidget t m, NFunctor a)
               => FieldWidgetConfig t m r e a
               -> FieldWidget t m r e a
mkFieldWidget' (FieldWidgetConfig' v r) = FieldWidget' $ \i dr da ev -> mdo
  dErrors <- holdDyn [] $ leftmost [fmap _wiValue . NonEmpty.toList <$> eErrors, [] <$ eSuccesses]
  eChange' <- r i dr da dErrors ev
  let
    eChange = leftmost [eChange', current da <@ ev]
    eChecked = (\r' -> v i r') <$> current dr <@> eChange
    (eErrors, eSuccesses) = fanEither $ toEither . ntraverse (fmap Identity) <$> eChecked

  pure (const <$> eChange)

wrapFieldWidgetConfig :: MonadWidget t m
                      => FieldWidgetConfig' t m r e (f a) (g b)
                      -> FieldWidgetConfig' t m r e (Wrap a f) (Wrap b g)
wrapFieldWidgetConfig (FieldWidgetConfig' v' r') =
  let
    v i r a =
      Wrap $ v' i r (unWrap a)
    r i dr da de ev = do
      ea <- r' i dr (unWrap <$> da) de ev
      pure (Wrap <$> ea)
  in
    FieldWidgetConfig' v r

wrapFieldWidget :: MonadWidget t m => FieldWidget' t m r e (f a) -> FieldWidget' t m r e (Wrap a f)
wrapFieldWidget w = FieldWidget' $ \i dr da ev -> do
  ea <- runFieldWidget w i dr (unWrap <$> da) ev
  pure (dimap unWrap Wrap <$> ea)

mkFieldWidget :: MonadWidget t m
              => FieldWidgetConfig' t m r e (Maybe a) (Validation (NonEmpty (WithId e)) a)
              -> FieldWidget t m r e (Wrap a)
mkFieldWidget =
  mkFieldWidget' .
  wrapFieldWidgetConfig
  -- wrapFieldWidget . mkFieldWidget'

blankFieldWidget :: (Reflex t, Monad m) => FieldWidget t m r e a
blankFieldWidget = FieldWidget' $ \_ _ _ _ ->
  pure never

data Field t m r e a where
  Field :: NFunctor a'
        => (r -> a Maybe -> r')
        -> (Id -> Id)
        -> (forall f. Functor f => Lens' (a f) (a' f))
        -> FieldWidgetConfig t m r' e a'
        -> Field t m r e a

data Piece t m r e a s where
  FieldPiece  :: m s
              -> m s
              -> Field t m r e a
              -> Piece t m r e a s
  WidgetPiece :: m s
              -> Piece t m r e a s

nestValidate :: Field t m r e a
             -> (Id -> r -> a Maybe -> a (Validation (NonEmpty (WithId e))))
             -> (Id -> r -> a Maybe -> a (Validation (NonEmpty (WithId e))))
nestValidate (Field fr fi l (FieldWidgetConfig' v _)) fn i r a =
  set l (v (fi i) (fr r a) (view l a)) (fn i r a)

requiredNF :: (NFunctor a, AsNotSpecified e)
           => Id
           -> r
           -> a Maybe
           -> a (Validation (NonEmpty (WithId e)))
requiredNF i _ =
  runIdentity .
  ntraverse (Identity . maybe (Failure . pure . WithId i $ _NotSpecified # ()) Success)

  --   _fwcValidate :: Id -> r -> a -> b
  -- , _fwcRender :: Id -> Dynamic t r -> Dynamic t a -> Dynamic t [e] -> Event t () -> m (Event t a)
nest :: (MonadWidget t m, NFunctor a)
     => (Id -> r -> a Maybe -> a (Validation (NonEmpty (WithId e))))
     -> ((Field t m r e a -> EventWriterT t (Endo (a Maybe)) m ()) -> EventWriterT t (Endo (a Maybe)) m ())
     -> FieldWidgetConfig t m r e a
nest v mkN = FieldWidgetConfig' v $ \i dr da de ev -> mdo
  (_, eE) <- runEventWriterT . mkN $ \fp -> case fp of
    Field ctx iFn l fwc@(FieldWidgetConfig' _ w) -> do
      ea <- lift $
        runFieldWidget
          (mkFieldWidget' fwc)
          (iFn i)
          (ctx <$> dr <*> da)
          (view l <$> da)
          ev
      tellEvent $ Endo . over l <$> ea
  let
    eChange = flip ($) <$> current da <@> (appEndo <$> eE)
  pure eChange


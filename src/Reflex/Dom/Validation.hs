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
matchOrDescendant i1 i2 = i1 == i2 || Just i1 == view idParent i2

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

class AsCompleted f where
  completed :: Lens' (f g) (Wrap Bool g)

instance AsCompleted (Wrap Bool) where
  completed = id

completeW :: (MonadWidget t m, HasErrorMessage e)
          => ValidationWidget t m e (Wrap Bool)
completeW i dv des = divClass "form-group" $ do
  el "label" $ text "Complete"

  let
    it = idToText i

  evTrue <- divClass "form-check" $ do
    let
      itt = it <> "-true"
      f = fromMaybe False . unWrap
      dv' = f <$> dv
    iv <- sample . current $ dv'
    let ev = updated dv'

    cb <- checkbox iv $ def
      & setValue .~ ev
      & attributes .~ pure ("id" =: itt)  <>
                    (("class" =:) . ("form-check-input " <>) . bool "is-invalid" "is-valid" . null <$> des)
    elAttr "label" ("class" =: "form-check-label" <> "for" =: itt) $ text "Complete"
    let ev' = cb ^. checkbox_change

    errorsForId i des

    pure $ bool Nothing (Just True) <$> ev'

  evFalse <- divClass "form-check" $ do
    let
      itf = it <> "-false"
      f = maybe False not . unWrap
      dv' = f <$> dv
    iv <- sample . current $ dv'
    let ev = updated dv'

    cb <- checkbox iv $ def
      & setValue .~ ev
      & attributes .~ pure ("id" =: itf)  <>
                    (("class" =:) . ("form-check-input " <>) . bool "is-invalid" "is-valid" . null <$> des)
    elAttr "label" ("class" =: "form-check-label" <> "for" =: itf) $ text "Incomplete"
    let ev' = cb ^. checkbox_change

    errorsForId i des

    pure $ bool Nothing (Just False) <$> ev'


  pure $ Endo . const . Wrap <$> leftmost [evTrue, evFalse]

completedF :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, AsCompleted f)
           => Field t m e f (Wrap Bool)
completedF = Field completed (\i -> Id (Just i) "-c") required completeW

class AsReason f where
  reason :: Lens' (f g) (Wrap (Maybe Text) g)

instance AsReason (Wrap (Maybe Text)) where
  reason = id

reasonV :: ValidationFn e (Wrap (Maybe Text)) (Wrap (Maybe Text))
reasonV _ (Wrap (Just (Just t))) =
  Success . Wrap . Identity $
    if Text.null t then Nothing else Just t
reasonV _ _ =
  Success . Wrap . Identity $
    Nothing

reasonW :: (MonadWidget t m, HasErrorMessage e)
        => ValidationWidget t m e (Wrap (Maybe Text))
reasonW i dv des = divClass "form-group" $ do
  let it = idToText i
  elAttr "label" ("for" =: it) $ text "Reason"

  let
    f = fromMaybe "" . join . unWrap
    dv' = f <$> dv
  iv <- sample . current $ dv'
  let ev = updated dv'
  ti <- textInput $ def
    & textInputConfig_initialValue .~ iv
    & setValue .~ ev
    & attributes .~ pure ("id" =: it) <>
                    (("class" =:) . ("form-control " <>) . bool "is-invalid" "is-valid" . null <$> des)
  let ev' = ti ^. textInput_input

  errorsForId i des

  pure $ Endo . const . Wrap . Just . (\t -> if Text.null t then Nothing else Just t) <$> ev'

reasonF :: (MonadWidget t m, HasErrorMessage e, AsReason f)
        => Field t m e f (Wrap (Maybe Text))
reasonF = Field reason (\i -> Id (Just i) "-r") reasonV reasonW

class HasReasonRequiredForIncompete e where
  _ReasonRequiredForIncomplete :: Prism' e ()

data CompletedWithReason f = CompletedWithReason {
    _cwrCompleted :: Wrap Bool f
  , _cwrReason :: Wrap (Maybe Text) f
  }

deriving instance (Eq (f Bool), Eq (f (Maybe Text))) => Eq (CompletedWithReason f)
deriving instance (Ord (f Bool), Ord (f (Maybe Text))) => Ord (CompletedWithReason f)
deriving instance (Show (f Bool), Show (f (Maybe Text))) => Show (CompletedWithReason f)
deriving instance (Read (f Bool), Read (f (Maybe Text))) => Read (CompletedWithReason f)

makeLenses ''CompletedWithReason

instance AsCompleted CompletedWithReason where
  completed = cwrCompleted

instance AsReason CompletedWithReason where
  reason = cwrReason

completedWithReasonV :: forall t m e. (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, HasReasonRequiredForIncompete e)
                     => Proxy t -> Proxy m -> ValidationFn e CompletedWithReason CompletedWithReason
completedWithReasonV _ _ i cr =
  let
    fC = completedF :: Field t m e CompletedWithReason (Wrap Bool)
    vC = fieldValidation fC i cr
    fR = reasonF :: Field t m e CompletedWithReason (Wrap (Maybe Text))
    vR = fieldValidation fR i cr
    f c r =
      if unwrapV c == False && unwrapV r == Nothing
      then Failure . pure . WithId (fieldId fR i) $ _ReasonRequiredForIncomplete # ()
      else Success $ CompletedWithReason c r
  in
    vC `bindValidation` \c ->
    vR `bindValidation` \r ->
    f c r

completedWithReasonW :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e)
                     => ValidationWidget t m e CompletedWithReason
completedWithReasonW i dv de = do
  eC <- fieldWidget completedF i dv de
  eR <- fieldWidget reasonF i dv de
  pure $ eC <> eR

  -- let
  --   wComplete = Workflow $ do
  --     e <- fieldWidget completedF i dv de
  --     eNext <- buttonClass "Next" "btn"
  --     pure (e, wReason <$ eNext)
  --   wReason = Workflow $ do
  --     e <- fieldWidget reasonF i dv de
  --     eBack <- buttonClass "Back" "btn"
  --     pure (e, wComplete <$ eBack)

  -- de <- workflow wComplete
  -- pure . switchDyn $ de

class AsCompletedWithReason f where
  completedWithReason :: Lens' (f g) (CompletedWithReason g)

instance AsCompletedWithReason CompletedWithReason where
  completedWithReason = id

completedWithReasonF :: forall t m e f. (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, HasReasonRequiredForIncompete e, AsCompletedWithReason f)
                     => Field t m e f CompletedWithReason
completedWithReasonF =
  Field completedWithReason (\i -> Id (Just i) "-cwr") (completedWithReasonV (Proxy :: Proxy t) (Proxy :: Proxy m)) completedWithReasonW

-- class AsToggleButton f where
--   toggleButton :: Lens' (f g) (Wrap Bool g)

-- instance AsToggleButton (Wrap Bool) where
--   toggleButton = id

-- rework toggle into a todo item

data TodoItem f =
  TodoItem {
    _tiComplete :: Wrap Bool f
  , _tiItem :: Wrap Text f
  }

makeLenses ''TodoItem

completeF :: MonadWidget t m => Field t m e TodoItem (Wrap Bool)
completeF = Field tiComplete (\i -> Id (Just i) "-c") toggleV toggleW

-- TODO make this an error if it is empty
itemV :: ValidationFn e (Wrap Text) (Wrap Text)
itemV i mv =
  Success . Wrap . Identity . fromMaybe "" . unWrap $ mv

itemW :: MonadWidget t m
      => ValidationWidget t m e (Wrap Text)
itemW i dv _ = do
  let
    f = fromMaybe "" . unWrap
    ev = f <$> updated dv
  iv <- fmap f . sample . current $ dv
  ti <- textInput $ def
    & setValue .~ ev
    & textInputConfig_initialValue .~ iv
  let ev' = ti ^. textInput_input

  pure $ Endo . const . Wrap . Just <$> ev'

itemF :: MonadWidget t m => Field t m e TodoItem (Wrap Text)
itemF = Field tiItem (\i -> Id (Just i) "-i") itemV itemW

todoItemV :: forall t m e. MonadWidget t m => Proxy t -> Proxy m -> ValidationFn e TodoItem TodoItem
todoItemV _ _ i mti =
  TodoItem <$> fieldValidation (completeF @t @m) i mti <*> fieldValidation (itemF @t @m) i mti

todoItemW :: (MonadWidget t m, HasErrorMessage e)
          => ValidationWidget t m e TodoItem
todoItemW i dv des = do

  eComplete <- fieldWidget completeF i dv des
  eItem <- fieldWidget itemF i dv des

  errorsForId i des

  pure $ eComplete <> eItem

todoItemF :: forall t m e. (MonadWidget t m, HasErrorMessage e) => Field t m e TodoItem TodoItem
todoItemF = Field id (\i -> Id (Just i) "-ti") (todoItemV (Proxy :: Proxy t) (Proxy :: Proxy m)) todoItemW

toggleV :: ValidationFn e (Wrap Bool) (Wrap Bool)
toggleV i mv =
  Success . Wrap . Identity . fromMaybe False . unWrap $ mv

toggleW :: (MonadWidget t m)
        => ValidationWidget t m e (Wrap Bool)
toggleW i dv _ = do
  let
    f = fromMaybe False . unWrap
    ev = f <$> updated dv
  iv <- fmap f . sample . current $ dv
  cb <- checkbox iv $ def
    & setValue .~ ev
    & attributes .~ pure ("id" =: idToText i)
  let ev' = cb ^. checkbox_change

  pure $ Endo . const . Wrap . Just <$> ev'

toggleF :: (MonadWidget t m)
        => Field t m e (Wrap Bool) (Wrap Bool)
toggleF =
  Field id (\i -> Id (Just i) "-t") toggleV toggleW

class HasCollectionKey k where
  keyId :: k -> Text

instance HasCollectionKey Int where
  keyId = Text.pack . show

-- maybe put these into an intmap, and require that we can pull a k out of `f' whatever`
liftV :: (Ord k, HasCollectionKey k)
      => Field t m e f f' -- ValidationFn e f f'
      -> ValidationFn e (Compose (Map k) f) (Compose (Map k) f')
liftV (Field l fi vfn _) i =
 fmap Compose .
 Map.traverseWithKey (\k v -> vfn (fi (Id (Just i) ("-" <> keyId k))) (view l v)) .
 getCompose

liftW :: forall t m e f k. (MonadWidget t m, HasErrorMessage e, Num k, Enum k, Ord k, HasCollectionKey k)
      => Field t m e f f
      -> m (Event t (f Maybe))
      -> m (Event t ())
      -> ValidationWidget t m e (Compose (Map k) f)
liftW (Field l fi _ fw) addMe deleteMe i dv des = do
  -- TODO add buttons to each row to allow them to move up and down

  eAdd <- addMe

  dme <- listWithKey (getCompose <$> dv) $ \k dv' -> do
    let i' = Id (Just i) ("-" <> keyId k)
    divClass "form-group" $ do
      eEl <- fw (fi i') (view l <$> dv') $ filter (matchOrDescendant i' . view wiId) <$> des
      eDel <- deleteMe
      pure (eEl, eDel)

  errorsForId i des

  let
    mapEndo :: k -> Endo (f Maybe) -> Endo (Compose (Map k) f Maybe)
    mapEndo k v = Endo $ Compose . Map.adjust (appEndo v) k . getCompose
    eChanges = fmap (foldMap id . Map.mapWithKey mapEndo) . switchDyn . fmap (mergeMap . fmap fst) $ dme
    eDeletes = fmap (\ks -> Endo $ Compose . (\m -> foldr Map.delete m . Map.keys $ ks) . getCompose) . switchDyn . fmap (mergeMap . fmap snd) $ dme
    eAdditions = (\v -> Endo $ Compose . (\m -> Map.insert (maybe 0 (succ . fst . fst) . Map.maxViewWithKey $ m) v m) . getCompose) <$> eAdd

  pure $ eChanges <> eDeletes <> eAdditions

liftF :: forall t m e f f' k. (MonadWidget t m, HasErrorMessage e, Num k, Enum k, Ord k, HasCollectionKey k)
      => (forall g. Functor g => Lens' (f g) (Compose (Map k) f' g))
      -> (Id -> Id)
      -> Field t m e f' f'
      -> m (Event t (f' Maybe))
      -> m (Event t ())
      -> Field t m e f (Compose (Map k) f')
liftF l fi f addMe deleteMe =
  Field l (\i -> Id (Just (fi i)) "-xs") (liftV f) (liftW f addMe deleteMe)

class AsToggles f where
  toggleButtons :: Lens' (f g) (Compose (Map Int) (Wrap Bool) g)

class AsTodoItems f where
  todoItems :: Lens' (f g) (Compose (Map Int) TodoItem g)

-- TODO add a text field here to put things through their paces
togglesF :: (MonadWidget t m, HasErrorMessage e, AsTodoItems f) => Field t m e f (Compose (Map Int) TodoItem)
togglesF =
  let
    addMe =
      divClass "form-group" $ do
        ti <- textInput def
        eClick <- buttonClass "Add" "btn"
        pure $ (\t -> TodoItem (Wrap (Just False)) (Wrap (Just t))) <$> current (value ti) <@ eClick
    deleteMe =
      buttonClass "Remove" "btn"
  in
    liftF todoItems (\i -> Id (Just i) "-ts") todoItemF addMe deleteMe

data TestCollections f =
  TestCollections {
    _tcCompletedWithReason :: CompletedWithReason f
  , _tcTodoItems :: Map Int (TodoItem f)
  }

makeLenses ''TestCollections

instance AsCompletedWithReason TestCollections where
  completedWithReason = tcCompletedWithReason

instance AsTodoItems TestCollections where
  todoItems = tcTodoItems . _Unwrapped

testCollectionsV :: forall t m e.
                 ( MonadWidget t m
                 , HasErrorMessage e
                 , HasNotSpecified e
                 , HasReasonRequiredForIncompete e
                 , HasCollectionTooSmall e
                 )
                 => Proxy t
                 -> Proxy m
                 -> ValidationFn e TestCollections TestCollections
testCollectionsV _ _ i tc =
  let
    tf = togglesF @t @m @e @TestCollections
  in
    TestCollections <$>
      fieldValidation (completedWithReasonF @t @m) i tc <*>
      (fieldValidation tf i tc) `bindValidation`
        (\(Compose xs) -> if length xs < 2
                then (Failure . pure . WithId (fieldId tf i) $ _CollectionTooSmall # ())
                else Success xs)

testCollectionsW :: (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, HasReasonRequiredForIncompete e)
                 => ValidationWidget t m e TestCollections
testCollectionsW i dv de =
  (<>) <$>
    fieldWidget completedWithReasonF i dv de <*>
    fieldWidget togglesF i dv de

class AsTestCollections f where
  testCollections :: Lens' (f g) (TestCollections g)

instance AsTestCollections TestCollections where
  testCollections = id

testCollectionsF :: forall t m e f. (MonadWidget t m, HasErrorMessage e, HasNotSpecified e, HasReasonRequiredForIncompete e, HasCollectionTooSmall e, AsTestCollections f)
                     => Field t m e f TestCollections
testCollectionsF =
  Field testCollections (\i -> Id (Just i) "-tc") (testCollectionsV (Proxy :: Proxy t) (Proxy :: Proxy m)) testCollectionsW

class HasCollectionTooSmall e where
  _CollectionTooSmall :: Prism' e ()

data MyError =
    MENotSpecified
  | MEReasonRequiredForIncomplete
  | MECollectionTooSmall
  deriving (Eq, Ord, Show, Read)

makePrisms ''MyError

instance HasErrorMessage MyError where
  errorMessage MENotSpecified = "Not specified"
  errorMessage MEReasonRequiredForIncomplete = "Reason required when not complete"
  errorMessage MECollectionTooSmall = "Collection too small"

instance HasNotSpecified MyError where
  _NotSpecified = _MENotSpecified

instance HasReasonRequiredForIncompete MyError where
  _ReasonRequiredForIncomplete = _MEReasonRequiredForIncomplete

instance HasCollectionTooSmall MyError where
  _CollectionTooSmall = _MECollectionTooSmall

-- this puts a potential validation button at the bottom, which might not be what we want in all cases
wrapUp :: MonadWidget t m => Field t m e f f -> f Maybe -> (Dynamic t (f Maybe) -> m (Event t (f Maybe))) -> m (Event t (f Identity))
wrapUp f ini v = mdo
  let i = Id Nothing "top"

  dcr <- foldDyn ($) ini $ fmap appEndo eFn
  eFn <- fieldWidget f i dcr des
  eV <- v dcr
  let (eFailure, eSuccess) = fanEither $ toEither . fieldValidation f i <$> eV
  -- TODO printing these failures would be interesting
  des :: Dynamic t [WithId e] <- holdDyn [] . leftmost $ [NonEmpty.toList <$> eFailure, [] <$ eSuccess]

  pure eSuccess

-- TODO do we always want changes to be saved?
-- - at the moment, any change causes the main dynamic to change
-- - we might want to tie that to buttons in workflows etc..., and so making that configurable might be a thing

go :: forall t m. MonadWidget t m => m (Event t (TestCollections Identity))
go = wrapUp (testCollectionsF :: Field t m MyError TestCollections TestCollections) (TestCollections (CompletedWithReason (Wrap Nothing) (Wrap Nothing)) mempty) $ \d -> do

  -- the version with a validation button
  eValidate <- buttonClass "Validate" "btn"
  pure (current d <@ eValidate)

  -- the version where every change causes a validation
  -- pure (updated d)

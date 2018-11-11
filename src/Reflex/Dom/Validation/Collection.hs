{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.Dom.Validation.Collection (
    collectionF
  ) where

import Control.Monad (join)
import Data.Bool (bool)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Endo(..))
import Data.Foldable (fold)
import Data.Functor.Compose (Compose(..))

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import Reflex.Dom.Validation

import Reflex.Dom.Validation.Bootstrap.Errors

-- maybe put these into an intmap, and require that we can pull a k out of `f' whatever`
collectionV :: (Maybe k -> Id -> Id)
            -> Field t m e f f' -- ValidationFn e f f'
            -> ValidationFn e (Compose (Map k) f) (Compose (Map k) f')
collectionV ki (Field l fi vfn _) i =
 fmap Compose .
 Map.traverseWithKey (\k v -> vfn (fi (ki (Just k) i)) (view l v)) .
 getCompose

gatherCollectionEvents :: forall t e f k. (Reflex t, Num k, Enum k, Ord k)
                       => Event t (f Maybe)
                       -> Dynamic t (Map k (ValidationWidgetOutput t e f, Event t ()))
                       -> ValidationWidgetOutput t e (Compose (Map k) f)
gatherCollectionEvents eAdd dme =
  let
    mapEndo :: k -> Endo (f Maybe) -> Endo (Compose (Map k) f Maybe)
    mapEndo k v = Endo $ Compose . Map.adjust (appEndo v) k . getCompose

    dFailures :: Dynamic t [WithId e]
    dFailures = fmap (join . Map.elems) . joinDynThroughMap . fmap (fmap (_vwoFailures . fst)) $ dme

    eChanges :: Event t (Endo (Compose (Map k) f Maybe))
    eChanges = fmap (fold . Map.mapWithKey mapEndo) . switchDyn . fmap (mergeMap . fmap (_vwoSuccesses . fst)) $ dme

    eDeletes :: Event t (Endo (Compose (Map k) f Maybe))
    eDeletes = fmap (\ks -> Endo $ Compose . (\m -> foldr Map.delete m . Map.keys $ ks) . getCompose) . switchDyn . fmap (mergeMap . fmap snd) $ dme

    eAdditions :: Event t (Endo (Compose (Map k) f Maybe))
    eAdditions = (\v -> Endo $ Compose . (\m -> Map.insert (maybe 0 (succ . fst . fst) . Map.maxViewWithKey $ m) v m) . getCompose) <$> eAdd
  in
    ValidationWidgetOutput
      dFailures
      (eChanges <> eDeletes <> eAdditions)

-- TODO generalize the bootstrap specific bits of this away
collectionW :: forall t m e f k. (MonadWidget t m, HasErrorMessage e, Num k, Enum k, Ord k)
      => (Maybe k -> Id -> Id)
      -> Field t m e f f
      -> m (Event t (f Maybe))
      -> m (Event t ())
      -> ValidationWidget t m e (Compose (Map k) f)
collectionW ki (Field l fi _ fw) addMe deleteMe i dv des =
  let
    dClass = ("form-control " <>) <$> errorClass i des
  in do
    vwo <- elDynClass "div" dClass $ do
      -- TODO add buttons to each row to allow them to move up and down
      eAdd <- addMe

      dm <- listWithKey (getCompose <$> dv) $ \k dv' -> do
        let i' = ki (Just k) i
        el "div" $ do
        -- divClass "form-group" $ do
          vwo' <- fw (fi i') (view l <$> dv') $ filter (matchOrDescendant i' . view wiId) <$> des
          eDel <- deleteMe
          pure (vwo', eDel)

      pure $ gatherCollectionEvents eAdd dm

    errorsForId i des
    pure vwo

collectionF :: forall t m e f f' k. (MonadWidget t m, NFunctor f', HasErrorMessage e, Num k, Enum k, Ord k)
      => (forall g. Functor g => Lens' (f g) (Compose (Map k) f' g))
      -> (Maybe k -> Id -> Id)
      -> Field t m e f' f'
      -> m (Event t (f' Maybe))
      -> m (Event t ())
      -> Field t m e f (Compose (Map k) f')
collectionF l fi f addMe deleteMe =
  Field l (fi Nothing) (collectionV fi f) (collectionW fi f addMe deleteMe)

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
import Reflex.Dom.Validation.Classes
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Error

import Reflex.Dom.Validation.Bootstrap.Errors

-- maybe put these into an intmap, and require that we can pull a k out of `f' whatever`
collectionV :: Ord k
            => (Maybe k -> Id -> Id)
            -> Field t m e f f' u u' v v' -- ValidationFn e f f'
            -> ValidationFn e (Map k v) (Compose (Map k) f) (Compose (Map k) f')
collectionV ki (Field l _ lc fi vfn _) = toValidationFn $ \i c ->
 fmap Compose .
 -- TODO this should fail if c does not have the key k
 Map.traverseWithKey (\k v -> runValidationFn vfn (fi (ki (Just k) i)) (lc v (c Map.! k)) (view l v)) .
 getCompose

gatherCollectionEvents :: forall t e f u k. (Reflex t, Num k, Enum k, Ord k)
                       => Event t (f Maybe, u)
                       -> Dynamic t (Map k (ValidationWidgetOutput t e f u, Event t ()))
                       -> ValidationWidgetOutput t e (Compose (Map k) f) (Map k u)
gatherCollectionEvents eAdd dme =
  let
    mapEndo :: k -> Endo (f Maybe) -> Endo (Compose (Map k) f Maybe)
    mapEndo k v = Endo $ Compose . Map.adjust (appEndo v) k . getCompose

    mapEndoU :: k -> Endo u -> Endo (Map k u)
    mapEndoU k u = Endo $ Map.adjust (appEndo u) k

    dFailures :: Dynamic t [WithId e]
    dFailures = fmap (join . Map.elems) . joinDynThroughMap . fmap (fmap (_vwoFailures . fst)) $ dme

    eChanges :: Event t (Endo (Compose (Map k) f Maybe))
    eChanges = fmap (fold . Map.mapWithKey mapEndo) . switchDyn . fmap (mergeMap . fmap (_vwoSuccesses . fst)) $ dme

    eChangesU :: Event t (Endo (Map k u))
    eChangesU = fmap (fold . Map.mapWithKey mapEndoU) . switchDyn . fmap (mergeMap . fmap (_vwoUI . fst)) $ dme

    eDeletes :: Event t (Endo (Compose (Map k) f Maybe))
    eDeletes = fmap (\ks -> Endo $ Compose . (\m -> foldr Map.delete m . Map.keys $ ks) . getCompose) . switchDyn . fmap (mergeMap . fmap snd) $ dme

    eDeletesU :: Event t (Endo (Map k u))
    eDeletesU = fmap (\ks -> Endo $ (\m -> foldr Map.delete m . Map.keys $ ks)) . switchDyn . fmap (mergeMap . fmap snd) $ dme

    eAdditions :: Event t (Endo (Compose (Map k) f Maybe))
    eAdditions = (\v -> Endo $ Compose . (\m -> Map.insert (maybe 0 (succ . fst . fst) . Map.maxViewWithKey $ m) v m) . getCompose) . fst <$> eAdd

    eAdditionsU :: Event t (Endo (Map k u))
    eAdditionsU = (\v -> Endo $ (\m -> Map.insert (maybe 0 (succ . fst . fst) . Map.maxViewWithKey $ m) v m)) . snd <$> eAdd

  in
    ValidationWidgetOutput
      dFailures
      (eChanges <> eDeletes <> eAdditions)
      (eChangesU <> eDeletesU <> eAdditionsU)

-- TODO generalize the bootstrap specific bits of this away
collectionW :: forall t m e f u v k. (MonadWidget t m, HasErrorMessage e, Num k, Enum k, Ord k)
      => (Maybe k -> Id -> Id)
      -> Field t m e f f u u v v
      -> m (Event t (f Maybe, u))
      -> m (Event t ())
      -> ValidationWidget t e (Compose (Map k) f) (Map k u) (Map k v) m ()
collectionW ki (Field l lu lc fi _ fw) addMe deleteMe = toValidationWidget_ $ \i dv du dc des ->
  let
    dClass = ("form-control " <>) <$> errorClass i des
    combineMaps :: Compose (Map k) f g -> Map k u -> Map k v -> Map k (f g, (u, v))
    combineMaps v u c = Map.intersectionWith (,) (getCompose v) (Map.intersectionWith (,) u c)
    combinedMaps = combineMaps <$> dv <*> du <*> dc
  in do
    vwo <- elDynClass "div" dClass $ do
      -- TODO add buttons to each row to allow them to move up and down
      eAdd <- addMe

      dm <- listWithKey combinedMaps $ \k dvuc' -> do
        let
          i' = ki (Just k) i
          dv' = fst <$> dvuc'
          du' = fst . snd <$> dvuc'
          dc' = snd . snd <$> dvuc'
        el "div" $ do
        -- divClass "form-group" $ do
          vwo' <- runValidationWidget_ fw (fi i') (view l <$> dv') (view lu <$> du') (lc <$> dv' <*> dc') $ filter (matchOrDescendant i' . view wiId) <$> des
          eDel <- deleteMe
          pure (vwo', eDel)

      pure $ gatherCollectionEvents eAdd dm

    _ <- runValidationWidget errorsForId i dv du dc des
    pure vwo

collectionF :: forall t m e f f' u u' v v' k. (MonadWidget t m, NFunctor f', HasErrorMessage e, Num k, Enum k, Ord k)
      => (forall g. Functor g => Lens' (f g) (Compose (Map k) f' g))
      -> Lens' u (Map k u')
      -> (forall g. Functor g => f g -> v -> Map k v')
      -> (Maybe k -> Id -> Id)
      -> Field t m e f' f' u' u' v' v'
      -> m (Event t (f' Maybe, u'))
      -> m (Event t ())
      -> Field t m e f (Compose (Map k) f') u (Map k u') v (Map k v')
collectionF l lu lv fi f addMe deleteMe =
  Field l lu lv (fi Nothing) (collectionV fi f) (collectionW fi f addMe deleteMe)

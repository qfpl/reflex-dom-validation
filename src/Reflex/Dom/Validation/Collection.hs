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
module Reflex.Dom.Validation.Collection (
    HasCollectionKey(..)
  , collectionF
  ) where

import Data.Bool (bool)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Endo(..))
import Data.Functor.Compose (Compose(..))

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import Reflex.Dom.Validation

class HasCollectionKey k where
  keyId :: k -> Text

instance HasCollectionKey Int where
  keyId = Text.pack . show

-- maybe put these into an intmap, and require that we can pull a k out of `f' whatever`
collectionV :: (Ord k, HasCollectionKey k)
      => Field t m e f f' -- ValidationFn e f f'
      -> ValidationFn e (Compose (Map k) f) (Compose (Map k) f')
collectionV (Field l fi vfn _) i =
 fmap Compose .
 Map.traverseWithKey (\k v -> vfn (fi (Id (Just i) ("-" <> keyId k))) (view l v)) .
 getCompose

-- TODO generalize the bootstrap specific bits of this away
collectionW :: forall t m e f k. (MonadWidget t m, HasErrorMessage e, Num k, Enum k, Ord k, HasCollectionKey k)
      => Field t m e f f
      -> m (Event t (f Maybe))
      -> m (Event t ())
      -> ValidationWidget t m e (Compose (Map k) f)
collectionW (Field l fi _ fw) addMe deleteMe i dv des =
  let
    dClass = ("form-control " <>) . bool "is-invalid" "is-valid" . null . filter ((== i) . view wiId) <$> des
  in do
    eRes <- elDynClass "div" dClass $ do
      -- TODO add buttons to each row to allow them to move up and down
      eAdd <- addMe

      dme <- listWithKey (getCompose <$> dv) $ \k dv' -> do
        let i' = Id (Just i) ("-" <> keyId k)
        el "div" $ do
        -- divClass "form-group" $ do
          eEl <- fw (fi i') (view l <$> dv') $ filter (matchOrDescendant i' . view wiId) <$> des
          eDel <- deleteMe
          pure (eEl, eDel)

      let
        mapEndo :: k -> Endo (f Maybe) -> Endo (Compose (Map k) f Maybe)
        mapEndo k v = Endo $ Compose . Map.adjust (appEndo v) k . getCompose
        eChanges :: Event t (Endo (Compose (Map k) f Maybe))
        eChanges = fmap (foldMap id . Map.mapWithKey mapEndo) . switchDyn . fmap (mergeMap . fmap fst) $ dme
        eDeletes :: Event t (Endo (Compose (Map k) f Maybe))
        eDeletes = fmap (\ks -> Endo $ Compose . (\m -> foldr Map.delete m . Map.keys $ ks) . getCompose) . switchDyn . fmap (mergeMap . fmap snd) $ dme
        eAdditions :: Event t (Endo (Compose (Map k) f Maybe))
        eAdditions = (\v -> Endo $ Compose . (\m -> Map.insert (maybe 0 (succ . fst . fst) . Map.maxViewWithKey $ m) v m) . getCompose) <$> eAdd

      pure $ eChanges <> eDeletes <> eAdditions

    errorsForId i des
    pure eRes

collectionF :: forall t m e f f' k. (MonadWidget t m, HasErrorMessage e, Num k, Enum k, Ord k, HasCollectionKey k)
      => (forall g. Functor g => Lens' (f g) (Compose (Map k) f' g))
      -> (Id -> Id)
      -> Field t m e f' f'
      -> m (Event t (f' Maybe))
      -> m (Event t ())
      -> Field t m e f (Compose (Map k) f')
collectionF l fi f addMe deleteMe =
  Field l (\i -> Id (Just (fi i)) "-xs") (collectionV f) (collectionW f addMe deleteMe)

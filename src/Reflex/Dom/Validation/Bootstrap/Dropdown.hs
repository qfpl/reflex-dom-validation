{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
module Reflex.Dom.Validation.Bootstrap.Dropdown where

import Control.Monad (forM_, forM)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))

import Control.Lens

import Data.Text (Text)

import Data.Set (Set)
import qualified Data.Set as Set

import Reflex.Dom.Core

import Reflex.Dom.Validation

import Reflex.Dom.Validation.Bootstrap.Errors

data SelectOptionConfig c =
  SelectOptionConfig {
    _socLabel :: Text
  , _socValue :: c
  }

makeLenses ''SelectOptionConfig

data SelectWidgetConfig c =
  SelectWidgetConfig {
    _swcLabel :: Maybe Text
  , _swcValues :: [SelectOptionConfig c]
  }

makeLenses ''SelectWidgetConfig

selectWidget :: (MonadWidget t m, HasErrorMessage e, Eq c)
  => SelectWidgetConfig c
  -> ValidationWidget t m e (Wrap c)
selectWidget swc i dv des = divClass "form-group" $ do
  let
    it = idToText i

  forM_ (swc ^. swcLabel) $
    elAttr "label" ("for" =: it) . text

  let
    dAttrs = pure ("id" =: it) <> (("class" =:) . ("form-control " <>) <$> errorClass i des)
  eChanges <- elDynAttr "select" dAttrs $
    forM (swc ^. swcValues) $ \(SelectOptionConfig k v) -> do
      (elOption, _) <- elDynAttr' "option" (bool mempty ("selected" =: "") . (== (Just v)) . unWrap <$> dv) $ text k
      pure $ (Endo . const . Wrap . Just $ v) <$ domEvent Change elOption

  errorsForId i des

  pure $ ValidationWidgetOutput (pure mempty) (mergeWith (<>) eChanges)

selectOptionalWidget :: (MonadWidget t m, HasErrorMessage e, Eq c)
  => SelectWidgetConfig c
  -> ValidationWidget t m e (Wrap (Maybe c))
selectOptionalWidget swc i dv des = divClass "form-group" $ do
  let
    it = idToText i

  forM_ (swc ^. swcLabel) $
    elAttr "label" ("for" =: it) . text

  let
    dAttrs = pure ("id" =: it) <> (("class" =:) . ("form-control " <>) <$> errorClass i des)
  eChanges <- elDynAttr "select" dAttrs $
    forM (swc ^. swcValues) $ \(SelectOptionConfig k v) -> do
      (elOption, _) <- elDynAttr' "option" (bool mempty ("selected" =: "") . (== (Just (Just v))) . unWrap <$> dv) $ text k
      pure $ (Endo . const . Wrap . Just . Just $ v) <$ domEvent Change elOption

  errorsForId i des

  pure $ ValidationWidgetOutput (pure mempty) (mergeWith (<>) eChanges)

toggleSet :: Ord c
          => c
          -> Wrap (Set c) Maybe
          -> Endo (Wrap (Set c) Maybe)
toggleSet c (Wrap Nothing) =
  Endo . const . Wrap . Just . Set.singleton $ c
toggleSet c (Wrap (Just ss))
  | Set.member c ss =
    Endo $ \(Wrap mss) -> Wrap . maybe Nothing ((\s -> bool Nothing (Just s) (Set.null s)) . Set.delete c) $ mss
  | otherwise =
    Endo $ \(Wrap mss) -> Wrap . Just . Set.insert c . fromMaybe mempty $ mss

selectMultipleWidget :: (MonadWidget t m, HasErrorMessage e, Ord c)
  => SelectWidgetConfig c
  -> ValidationWidget t m e (Wrap (Set c))
selectMultipleWidget swc i dv des = divClass "form-group" $ do
  let
    it = idToText i

  forM_ (swc ^. swcLabel) $
    elAttr "label" ("for" =: it) . text

  let
    dAttrs = pure ("id" =: it <> "multiple" =: "") <> (("class" =:) . ("form-control " <>) <$> errorClass i des)
  eChanges <- elDynAttr "select" dAttrs $
    forM (swc ^. swcValues) $ \(SelectOptionConfig k v) -> do
      (elOption, _) <- elDynAttr' "option" (bool mempty ("selected" =: "") . maybe False (Set.member v) . unWrap <$> dv) $ text k
      pure $ toggleSet v <$> current dv <@ domEvent Change elOption

  errorsForId i des

  pure $ ValidationWidgetOutput (pure mempty) (mergeWith (<>) eChanges)

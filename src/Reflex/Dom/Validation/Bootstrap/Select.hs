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
module Reflex.Dom.Validation.Bootstrap.Select where

import Control.Monad (forM_, forM, join)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Monoid (Endo(..))

import Control.Lens

import Data.Text (Text)

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Map as Map

import Reflex.Dom.Core

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Error
import Reflex.Dom.Validation.Id
import Reflex.Dom.Validation.Wrap

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

selectWidget :: (MonadWidget t m, HasErrorMessage e, Ord c)
  => c
  -> SelectWidgetConfig c
  -> ValidationWidget t m e (Wrap c) u
selectWidget defV swc i dv du des = divClass "form-group" $ do
  let
    it = idToText i

  forM_ (swc ^. swcLabel) $
    elAttr "label" ("for" =: it) . text

  let
    m = Map.fromList . fmap (\soc -> (soc ^. socValue, soc ^. socLabel)) $ swc ^. swcValues
    dAttrs = pure ("id" =: it) <> (("class" =:) . ("form-control " <>) <$> errorClass i des)

  iV <- fmap (fromMaybe defV . unWrap) . sample . current $ dv
  ePostBuild <- getPostBuild
  dd <- dropdown iV (pure m) $ def
    -- & setValue .~ (fmap (fromMaybe defV . unWrap) . updated $ dv)
    & attributes .~ dAttrs

  errorsForId i des

  let
    eChange =
      fmap (Endo . const . Wrap . Just) . leftmost $
      [ dd ^. dropdown_change
      , iV <$ ePostBuild
      ]

  pure $
    ValidationWidgetOutput (pure mempty) eChange never

selectOptionalWidget :: (MonadWidget t m, HasErrorMessage e, Ord c)
  => SelectWidgetConfig c
  -> ValidationWidget t m e (Wrap (Maybe c)) u
selectOptionalWidget swc i dv du des = divClass "form-group" $ do
  let
    it = idToText i

  forM_ (swc ^. swcLabel) $
    elAttr "label" ("for" =: it) . text

  let
    m = Map.insert Nothing "" . Map.fromList . fmap (\soc -> (Just (soc ^. socValue), soc ^. socLabel)) $ swc ^. swcValues
    dAttrs = pure ("id" =: it) <> (("class" =:) . ("form-control " <>) <$> errorClass i des)

  iV <- fmap (join . unWrap) . sample . current $ dv
  dd <- dropdown iV (pure m) $ def
    & attributes .~ dAttrs

  let
    eChange = Endo . const . Wrap . Just <$> dd ^. dropdown_change

  errorsForId i des

  pure $ ValidationWidgetOutput (pure mempty) eChange never

-- this is going to require a reworking of dropdown from reflex-dom to get going

-- toggleSet :: Ord c
--           => c
--           -> Wrap (Set c) Maybe
--           -> Endo (Wrap (Set c) Maybe)
-- toggleSet c (Wrap Nothing) =
--   Endo . const . Wrap . Just . Set.singleton $ c
-- toggleSet c (Wrap (Just ss))
--   | Set.member c ss =
--     Endo $ \(Wrap mss) -> Wrap . maybe Nothing ((\s -> bool Nothing (Just s) (Set.null s)) . Set.delete c) $ mss
--   | otherwise =
--     Endo $ \(Wrap mss) -> Wrap . Just . Set.insert c . fromMaybe mempty $ mss

-- selectMultipleWidget :: (MonadWidget t m, HasErrorMessage e, Ord c)
--   => SelectWidgetConfig c
--   -> ValidationWidget t m e (Wrap (Set c))
-- selectMultipleWidget swc i dv des = divClass "form-group" $ do
--   let
--     it = idToText i

--   forM_ (swc ^. swcLabel) $
--     elAttr "label" ("for" =: it) . text

--   let
--     dAttrs = pure ("id" =: it <> "multiple" =: "") <> (("class" =:) . ("form-control " <>) <$> errorClass i des)
--   eChanges <- elDynAttr "select" dAttrs $
--     forM (swc ^. swcValues) $ \(SelectOptionConfig k v) -> do
--       (elOption, _) <- elDynAttr' "option" (bool mempty ("selected" =: "") . maybe False (Set.member v) . unWrap <$> dv) $ text k
--       pure $ toggleSet v <$> current dv <@ domEvent Change elOption

--   errorsForId i des

--   pure $ ValidationWidgetOutput (pure mempty) (mergeWith (<>) eChanges)

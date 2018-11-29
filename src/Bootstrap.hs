{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
module Bootstrap (
    debugApp
  , card
  , dynButtonDynAttr
  , dynButtonAttr
  , dynButtonDynClass
  , dynButtonClass
  , buttonDynAttr
  , buttonAttr
  , buttonDynClass
  , buttonClass
  , bootstrapDropdown
  ) where

import Control.Monad.Fix (MonadFix)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))

import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import Language.Javascript.JSaddle.WebSockets (debugOr)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings) 

bootstrapCssFiles :: [Text]
bootstrapCssFiles = [
    "css/bootstrap.min.css"
  , "css/font-awesome.min.css"
  , "css/style.css"
  , "css/syntax.css"
  ]

bootstrapJsFiles :: [Text]
bootstrapJsFiles = [
    "js/jquery-3.2.1.slim.min.js"
  , "js/popper.min.js"
  , "js/bootstrap.min.js"
  ]

headSection :: Text -> Widget x ()
headSection title = do
  el "title" $ text title
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "meta" ("name" =: "viewport" <>
                 "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no") blank
  let
    stylesheet s =
      elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) blank
  traverse_ stylesheet bootstrapCssFiles

tailSection :: Widget x ()
tailSection =
  let
    script src =
      elAttr "script" ("src" =: src <> "defer" =: "") blank
  in
    traverse_ script bootstrapJsFiles

-- mkApp :: (forall x. Widget x ()) -> IO ()
-- mkApp w = mainWidgetWithHead headSection $ w >> tailSection

debugApp :: Int -> (forall x. Widget x ()) -> IO ()
debugApp p w = 
  debugOr p (mainWidgetWithHead (headSection "Test me") $ w >> tailSection) $ 
    staticApp $ defaultWebAppSettings "./assets"

card :: MonadWidget t m => m a -> m a
card = divClass "card m-2" . divClass "card-body"

dynButtonDynAttr :: MonadWidget t m => Dynamic t Text -> Dynamic t (Map Text Text) -> m (Event t ())
dynButtonDynAttr dLabel dAttrs = do
  let attrs = "type" =: "button"
  (e, _) <- elDynAttr' "button" (pure attrs <> dAttrs) $ dynText dLabel
  pure $ domEvent Click e

dynButtonAttr :: MonadWidget t m => Dynamic t Text -> Map Text Text -> m (Event t ())
dynButtonAttr dLabel attr =
  dynButtonDynAttr dLabel (pure attr)

dynButtonDynClass :: MonadWidget t m => Dynamic t Text -> Dynamic t Text -> m (Event t ())
dynButtonDynClass dLabel dClass =
  dynButtonDynAttr dLabel $ ("class" =:) <$> dClass

dynButtonClass :: MonadWidget t m => Dynamic t Text -> Text -> m (Event t ())
dynButtonClass dLabel clss =
  dynButtonDynClass dLabel (pure clss)

buttonDynAttr :: MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m (Event t ())
buttonDynAttr label =
  dynButtonDynAttr (pure label)

buttonAttr :: MonadWidget t m => Text -> Map Text Text -> m (Event t ())
buttonAttr label =
  dynButtonAttr (pure label)

buttonDynClass :: MonadWidget t m => Text -> Dynamic t Text -> m (Event t ())
buttonDynClass label =
  dynButtonDynClass (pure label)

buttonClass :: MonadWidget t m => Text -> Text -> m (Event t ())
buttonClass label =
  dynButtonClass (pure label)

bootstrapDropdown :: forall k t m. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Ord k)
                  => k
                  -> Dynamic t (Map k Text)
                  -> DropdownConfig t k
                  -> m (Dropdown t k)
bootstrapDropdown k0 options (DropdownConfig setK dAttrs) =
  divClass "dropdown" $ mdo
    let
      attrs =
        "class" =: "btn dropdown-toggle" <>
        "type" =: "button" <>
        "data-toggle" =: "dropdown" <>
        "aria-haspopup" =: "true" <>
        "aria-expanded" =: "false"
    (elShow, _) <- elDynAttr' "button" (pure attrs <> dAttrs) $
      dynText . fmap (fromMaybe "") $ flip Map.lookup <$> options <*> dButton
    let eShow = domEvent Click elShow
    dChangeShow <- toggle False eShow
    dShow <- holdDyn False . leftmost $ [
        updated dChangeShow
      , False <$ eButton
      ]
    let
      dClass = ("class" =:) . ("dropdown-menu" <>) . bool "" " show" <$> dShow
    deButtons <- elDynAttr "div" dClass $
        listWithKey options $ \k v -> do
          (elButton, _) <- elAttr' "a" ("class" =: "dropdown-item" <> "href" =: "#") $ dynText v
          pure $ k <$ domEvent Click elButton
    let eButton = fmap (head . Map.keys) . switchDyn . fmap mergeMap $ deButtons
    dButton <- holdDyn k0 $ leftmost [eButton, setK]
    pure $ Dropdown dButton eButton

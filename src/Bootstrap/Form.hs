{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bootstrap.Form where

import Control.Monad (void, forM_)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup(..), First(..))

import Control.Monad.Trans (lift)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Core

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Workflow

import Bootstrap

requiredText :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
             => Text
             -> FieldWidget t m r e (Wrap Text)
requiredText =
  mkFieldWidget .
  FieldWidgetConfig required .
  const .
  formText

formText :: (MonadWidget t m, HasErrorMessage e)
         => Text
         -> Id
         -> Maybe Text
         -> Event t (Maybe Text)
         -> Dynamic t [e]
         -> m (Event t (Maybe Text))
formText l i = wrapBootstrapFormInput l i $ \da ib eb -> do
  let
    mt = fromMaybe ""
    tm t = if Text.null t then Nothing else Just t

  ti <- textInput $
    def & textInputConfig_initialValue .~ mt ib
        & setValue .~ (mt <$> eb)
        & attributes .~ fmap (addClass "form-control") da

  let
    eValidate = void . ffilter not . updated . _textInput_hasFocus $ ti
    eChange = tm <$> current (value ti) <@ eValidate

  pure eChange

data RadioButtonConfig t =
  RadioButtonConfig {
    rbcId :: Text -- maybe leave this for the attributes?
  , rbcValue :: Text
  , rbcLabel :: Text
  , rbcAttributes :: Dynamic t (Map Text Text)
  }

radioButton :: MonadWidget t m
            => Text
            -> Bool
            -> Event t Bool
            -> Dynamic t (Map Text Text)
            -> RadioButtonConfig t
            -> m (Event t Bool)
radioButton name checked e dSetAttrs c = do
  -- TODO prune out bad attributes (checked at least, probably also name and value, maybe id)
  let permanentAttrs = "type" =: "radio" <> "name" =: name <> "id" =: rbcId c <> "value" =: rbcValue c
      dAttrs = Map.delete "checked" . Map.union permanentAttrs <$> rbcAttributes c <> dSetAttrs
  modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
  i <- inputElement $ def
    & inputElementConfig_initialChecked .~ checked
    & inputElementConfig_setChecked .~ e
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ Map.mapKeys (AttributeName Nothing) permanentAttrs
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
  return $ _inputElement_checkedChange i


data RadioButtonSetConfig t a =
  RadioButtonSetConfig {
    rbscName :: Text
  , rbcsButtons :: [(a, RadioButtonConfig t)]
  , rbcsInitialValue :: Maybe a
  , rbcsSetValue :: Event t a
  , rbcsAttributes :: Dynamic t (Map Text Text)
  }

data RadioButtonSet t a =
  RadioButtonSet {
    rbsValue :: Dynamic t (Maybe a)
  , rbsChange :: Event t a
  }

radioButtonSet :: (MonadWidget t m, Ord a)
               => RadioButtonSetConfig t a
               -> ((a -> EventWriterT t (First a) m ()) -> EventWriterT t (First a) m ())
               -> m (RadioButtonSet t a)
radioButtonSet c fn = do
  (_, ef) <- runEventWriterT $ fn $ \a ->
    case lookup a (rbcsButtons c) of
      Nothing -> pure ()
      Just rbc -> do
        e <- radioButton (rbscName c) (Just a == rbcsInitialValue c) (fmap (== a) . rbcsSetValue $ c) (rbcsAttributes c) rbc
        tellEvent $ First a <$ ffilter id e
  let e = getFirst <$> ef
  d <- holdDyn (rbcsInitialValue c) $ Just <$> e
  pure $ RadioButtonSet d e

formRadios :: (MonadWidget t m, Ord a, Show a, HasErrorMessage e)
           => [(Text, a)]
           -> Id
           -> Maybe a
           -> Event t (Maybe a)
           -> Dynamic t [e]
           -> m (Event t (Maybe a))
formRadios rs (Id i) ia ea de = divClass "form-group" $ do
  let
    f j (l, v) = (v, RadioButtonConfig ((i <>) . ("-" <>) . Text.pack . show $ j) (Text.pack . show $ v) l mempty)
    m = zipWith f [0..] rs
    c = RadioButtonSetConfig i m ia (fmapMaybe id ea) (("class" =:) <$> pure "form-check-input " <> bool "is-invalid" "is-valid" . null <$> de)
  rbs <- radioButtonSet c $ \fn -> forM_ m $ \(a, rbc) -> divClass "form-check" $ do
    fn a
    elAttr "label" ("class" =: "form-check-label" <> "for" =: rbcId rbc) $ text (rbcLabel rbc)

    void . simpleList de $
      divClass "invalid-feedback" .
        dynText . fmap errorMessage

  pure $ Just <$> rbsChange rbs

addClass' :: Text -> Text -> Text
addClass' cls classes =
  mconcat .
  (classes :) .
  bool [" ", cls] [] .
  elem cls .
  Text.words $
  classes

addClass :: Text -> Map Text Text -> Map Text Text
addClass cls =
  Map.alter (Just . maybe cls (addClass' cls)) "class"

wrapBootstrapFormInput :: (MonadWidget t m, HasErrorMessage e)
                       => Text
                       -> Id
                       -> (Dynamic t (Map Text Text) -> Maybe b -> Event t (Maybe b) -> m (Event t (Maybe b)))
                       -> Maybe b
                       -> Event t (Maybe b)
                       -> Dynamic t [e]
                       -> m (Event t (Maybe b))
wrapBootstrapFormInput l (Id i) widgetFn ib eb de =
  divClass "form-group" $ do
    elAttr "label" ("for" =: i) $ text l

    let
      dValidClass = ("class" =:) . bool "is-invalid" "is-valid" . null <$> de
      dAttrs = pure ("id" =: i) <> dValidClass
    eChange <- widgetFn dAttrs ib eb

    void . simpleList de $
      divClass "invalid-feedback" .
        dynText . fmap errorMessage

    pure eChange

listButtons :: MonadWidget t m
            => Bool
            -> Bool
            -> m (Event t WLDirection)
listButtons isFirst isLast = divClass "row d-flex justify-content-between" $ do
  eBack <- buttonClass "Back" $ "btn" <> bool "" " invisible" isFirst
  eNext <- buttonClass "Next" $ "btn" <> bool "" " invisible" isLast
  pure . leftmost $ [WLBack <$ eBack, WLNext <$ eNext]

toFormRow :: MonadWidget t m
          => m a
          -> m a
toFormRow =
  divClass "form-row" .
  divClass "col"

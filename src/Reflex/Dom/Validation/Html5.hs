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
module Reflex.Dom.Validation.Html5 where

import Control.Monad (join)
import Data.Bool (bool)
import Text.Read (readMaybe)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

import GHCJS.DOM.Types (MonadJSM, uncheckedCastTo)
import GHCJS.DOM.HTMLInputElement (checkValidity, getValidity)
import GHCJS.DOM.ValidityState

import Data.Validation
import Reflex.Dom.Core

import Reflex.Dom.Validation

data ValidityError =
    BadInput
  | CustomError
  | PatternMismatch
  | RangeUnderflow
  | RangeOverflow
  | StepMismatch
  | TooLong
  | TooShort
  | TypeMismatch
  | ValueMissing
  deriving (Eq, Ord, Show, Read)

makePrisms ''ValidityError

class HasValidityError e where
  _ValidityError :: Prism' e ValidityError

instance HasErrorMessage ValidityError where
  errorMessage BadInput = "Bad input"
  errorMessage CustomError = "Custom error"
  errorMessage PatternMismatch = "Pattern mismatch"
  errorMessage RangeUnderflow = "Range underflow"
  errorMessage RangeOverflow = "Range overflow"
  errorMessage StepMismatch = "Step mismatch"
  errorMessage TooLong = "Too long"
  errorMessage TooShort = "Too short"
  errorMessage TypeMismatch = "Type mismatch"
  errorMessage ValueMissing = "Value missing"

checkValid :: (MonadJSM m, HasValidityError e)
           => ValidityState
           -> m (Validation (NonEmpty e) ())
checkValid vs =
  let
    f v fn = fmap (bool [] (pure $ _ValidityError # v)) . fn $ vs
  in do
    v <- getValid vs
    if v
    then pure (Success ())
    else do
      xs <- sequence [
          f BadInput getBadInput
        , f CustomError getCustomError
        , f PatternMismatch getPatternMismatch
        , f RangeUnderflow getRangeUnderflow
        , f RangeOverflow getRangeOverflow
        , f StepMismatch getStepMismatch
        , f TooLong getTooLong
        , f TooShort getTooShort
        , f TypeMismatch getTypeMismatch
        , f ValueMissing getValueMissing
        ]
      pure . maybe (Success ()) Failure . NonEmpty.nonEmpty . join $ xs

data ValidInputConfig t e a =
  ValidInputConfig {
    vicToText :: a -> Text
  , vicFromText :: Text -> Validation (NonEmpty e) a
  , vicType :: Text
  , vicInitialValue :: Maybe a
  , vicSetValue :: Event t a
  , vicAttributes :: Dynamic t (Map Text Text)
  }

data ValidInput t e a =
  ValidInput {
    viValue :: Dynamic t (Validation (NonEmpty e) a)
  , viInput :: Event t (Validation (NonEmpty e) a)
  , viHasFocus :: Dynamic t Bool
  }

valid :: (MonadWidget t m, HasValidityError e, HasErrorMessage e, Show a)
      => ValidInputConfig t e a
      -> m (ValidInput t e a)
valid (ValidInputConfig vTo vFrom vType initial eSetValue dAttrs) = do
  modifyAttrs <- dynamicAttributesToModifyAttributes $ fmap (Map.insert "type" vType) dAttrs
  i <- inputElement $ def
    & inputElementConfig_initialValue .~ maybe "" vTo initial
    & inputElementConfig_setValue .~ (vTo <$> eSetValue)
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs

  let
    check x = do
      v <- (>>= checkValid) . getValidity . _inputElement_raw $ i
      pure $ vFrom x <* v

  let
    eUserChange = _inputElement_input i
    eValidate = current (_inputElement_value i) <@ (ffilter not . updated . _inputElement_hasFocus $ i)
  input' <- performEvent $ check <$> leftmost [eUserChange, eValidate]

  d <- holdDyn (vFrom "") input'
  -- display $ either (show . fmap errorMessage) show . toEither <$> d

  let initial' = maybe (Failure . pure $ _ValidityError # ValueMissing) Success initial
  updated' <- performEvent $ check <$> updated (_inputElement_value i)
  value' <- holdDyn initial' updated'

  -- display $ either (show . fmap errorMessage) show . toEither <$> value'

  return $ ValidInput
    value'
    input'
    (_inputElement_hasFocus i)

-- data NumberInputConfig t =
--   NumberInputConfig {
--     nicInitialValue :: Maybe Int
--   , nicSetValue :: Event t Int
--   , nicAttributes :: Dynamic t (Map Text Text)
--   }

-- data NumberInput t =
--   NumberInput {
--     niValue :: Dynamic t (Maybe Int)
--   , niInput :: Event t (Maybe Int)
--   , niHasFocus :: Dynamic t Bool
--   }

-- number :: MonadWidget t m => NumberInputConfig t -> m (NumberInput t)
-- number (NumberInputConfig initial eSetValue dAttrs) = do
--   modifyAttrs <- dynamicAttributesToModifyAttributes $ fmap (Map.insert "type" "number") dAttrs
--   i <- inputElement $ def
--     & inputElementConfig_initialValue .~ maybe "" (Text.pack . show) initial
--     & inputElementConfig_setValue .~ (Text.pack . show <$> eSetValue)
--     & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs

--   -- let
--   --   f = checkValidity . _inputElement_raw $ i
--   -- eV <- performEvent $ f <$ _inputElement_input i
--   -- dV <- holdDyn False eV
--   -- display dV

--   -- let
--   --   g = do
--   --     vs <- getValidity . _inputElement_raw $ i
--   --     checkValid vs
--   -- eVs <- performEvent $ g <$ _inputElement_input i
--   -- d <- holdDyn (Success ()) eVs
--   -- display d

--   return $ NumberInput
--     (readMaybe . Text.unpack <$> _inputElement_value i)
--     (readMaybe . Text.unpack <$> _inputElement_input i)
--     (_inputElement_hasFocus i)

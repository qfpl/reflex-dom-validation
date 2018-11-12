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
import Text.Read (readMaybe)

import Control.Lens
import Control.Error

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

import Data.Time.Calendar
import Data.Time.Format

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
    _vicToText :: a -> Text
  , _vicFromText :: Text -> Validation (NonEmpty e) a
  , _vicType :: Text
  , _vicInitialValue :: Maybe a
  , _vicSetValue :: Event t a
  , _vicAttributes :: Dynamic t (Map Text Text)
  }

makeLenses ''ValidInputConfig

data ValidInput t e a =
  ValidInput {
    _viValue :: Dynamic t (Validation (NonEmpty e) a)
  , _viInput :: Event t (Validation (NonEmpty e) a)
  , _viHasFocus :: Dynamic t Bool
  }

makeLenses ''ValidInput

valid :: (MonadWidget t m, HasValidityError e, HasErrorMessage e)
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

  let initial' = maybe (Failure . pure $ _ValidityError # ValueMissing) Success initial
  updated' <- performEvent $ check <$> updated (_inputElement_value i)
  value' <- holdDyn initial' updated'

  return $ ValidInput
    value'
    input'
    (_inputElement_hasFocus i)

newtype ValidInputConfigBuilder t m e a =
  ValidInputConfigBuilder {
    runValidInputConfigBuilder :: Dynamic t (Wrap a Maybe)
                               -> Dynamic t (Map Text Text)
                               -> m (ValidInputConfig t e a)
  }

-- probably want versions of these for Maybe Day, etc...
-- can add the required attribute for the non-Maybe versions

dayConfigBuilder :: (Reflex t, MonadHold t m, HasValidityError e)
                 => ValidInputConfigBuilder t m e Day
dayConfigBuilder = ValidInputConfigBuilder $ \dv dattrs -> do
  iv <- sample . current $ dv
  pure $ ValidInputConfig
    (Text.pack . formatTime defaultTimeLocale "%Y-%m-%d")
    (maybe (Failure . pure $ _ValidityError . _BadInput # ()) (Success . fst) . headMay . readSTime False defaultTimeLocale "%Y-%m-%d" . Text.unpack)
    "date"
    (unWrap iv)
    (fmapMaybe unWrap $ updated dv)
    dattrs

intConfigBuilder :: (Reflex t, MonadHold t m, HasValidityError e)
                 => ValidInputConfigBuilder t m e Int
intConfigBuilder = ValidInputConfigBuilder $ \dv dattrs -> do
  iv <- sample . current $ dv
  pure $ ValidInputConfig
    (Text.pack . show)
    (maybe (Failure . pure $ _ValidityError . _BadInput # ()) Success . readMaybe . Text.unpack)
    "number"
    (unWrap iv)
    (fmapMaybe unWrap $ updated dv)
    (pure ("placeholder" =: "0") <> dattrs)

decimalConfigBuilder :: (Reflex t, MonadHold t m, HasValidityError e)
                     => ValidInputConfigBuilder t m e Double
decimalConfigBuilder = ValidInputConfigBuilder $ \dv dattrs -> do
  iv <- sample . current $ dv
  pure $ ValidInputConfig
    (Text.pack . show)
    (maybe (Failure . pure $ _ValidityError . _BadInput # ()) Success . readMaybe . Text.unpack)
    "number"
    (unWrap iv)
    (fmapMaybe unWrap $ updated dv)
    (pure ("placeholder" =: "0.00" <> "step" =: "0.01") <> dattrs)

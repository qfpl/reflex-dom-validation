{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Demo where

import Control.Monad (join, void)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import Data.Monoid (Endo(..))

import GHC.Generics (Generic)

import Control.Monad.Trans (lift)

import Control.Lens

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Dependent.Map (Some(..))

import Data.GADT.Compare (GEq(..), GCompare(..), (:~:)(..), GOrdering(..))

import Data.Validation

import Reflex.Dom.Core

import Data.Aeson (ToJSON(..), FromJSON(..))

import Reflex.Dom.Validation
import Reflex.Dom.Validation.Workflow
import Reflex.Dom.Validation.Storage
import Reflex.Dom.Validation.Widget
import Bootstrap
import Bootstrap.Form

import Reflex.Dom.Storage.Base
import Data.GADT.Aeson

boolW :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
      => FieldWidget t m r e (Wrap Bool)
boolW =
  mkFieldWidget .
  FieldWidgetConfig required .
  const $
  formRadios [("True", True), ("False", False)]

class AsReasonNotGiven e where
  _ReasonNotGiven :: Prism' e ()

reasonW :: (MonadWidget t m, HasErrorMessage e, AsReasonNotGiven e)
        => FieldWidget t m (Wrap Bool Maybe) e (Wrap (Maybe Text))
reasonW =
  let
    validate (Wrap (Just False)) i v =
      let
        x = fromMaybe "" (join v)
      in
        if Text.null x
        then Failure . pure . WithId i $ _ReasonNotGiven # ()
        else Success . Identity . Just $ x
    validate _ _ _ =
      Success . Identity $ Nothing

    render dr i mmt emmt de =
      let
        hideMe = "hidden" =: ""
        dAttrs = bool mempty hideMe . fromMaybe True . unWrap <$> dr
      in
        elDynAttr "div" dAttrs $ do
          e <- formText "Reason" i (join mmt) (join <$> emmt) de
          pure $ fmap Just <$> e
  in
    mkFieldWidget $ FieldWidgetConfig validate render

data Complete f =
  Complete {
    _cComplete :: Wrap Bool f
  , _cReason :: Wrap (Maybe Text) f
  }

deriving instance Generic (Complete Maybe)
deriving instance Show (Complete Maybe)
deriving instance Show (Complete Identity)

instance ToJSON (Complete Maybe) where
instance FromJSON (Complete Maybe) where

instance NFunctor Complete where
  nempty = Complete nempty nempty
  ntraverse f (Complete b r) = Complete <$> ntraverse f b <*> ntraverse f r

makeLenses ''Complete

completeBool :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
             => Field t m r e Complete
completeBool =
  Field const (<> "-c") cComplete boolW

completeReason :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e, AsReasonNotGiven e)
               => Field t m r e Complete
completeReason =
  Field (const $ view cComplete) (<> "-r") cReason reasonW

completeW :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e, AsReasonNotGiven e)
            => FieldWidget t m r e Complete
completeW = nest $ \child ->
  el "fieldset" $ do
    el "legend" $ text "Complete"
    child completeBool
    child completeReason

data Nest f =
  Nest {
    _nComplete1 :: Complete f
  , _nComplete2 :: Complete f
  }

deriving instance Generic (Nest Maybe)
deriving instance Show (Nest Maybe)
deriving instance Show (Nest Identity)

instance ToJSON (Nest Maybe) where
instance FromJSON (Nest Maybe) where

instance NFunctor Nest where
  nempty = Nest nempty nempty
  ntraverse f (Nest c1 c2) = Nest <$> ntraverse f c1 <*> ntraverse f c2

makeLenses ''Nest

nestComplete1 :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e, AsReasonNotGiven e)
             => Field t m r e Nest
nestComplete1 =
  Field const (<> "-1") nComplete1 completeW

nestComplete2 :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e, AsReasonNotGiven e)
             => Field t m r e Nest
nestComplete2 =
  Field const (<> "-2") nComplete2 completeW

nestWList :: forall t m r e. (MonadWidget t m, HasErrorMessage e, AsNotSpecified e, AsReasonNotGiven e)
          => Proxy e
          -> FieldWidget t m r e Nest
nestWList _ =
  workflowList listButtons [
    workflowField nestComplete1
  , workflowWidget $ text "Half way"
  , workflowField nestComplete2
  , workflowWidget $ text "Done!"
  ]

nestW :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e, AsReasonNotGiven e)
      => Proxy e
      -> FieldWidget t m r e Nest
nestW _ = nest $ \child ->
  el "fieldset" $ do
    el "legend" $ text "Nested"
    -- n <- number $ NumberInputConfig (Just 3) never (pure $ "min" =: "2" <> "max" =: "5")
    child nestComplete1
    child nestComplete2

nestWWorkflow :: forall t m r e. (MonadWidget t m, HasErrorMessage e, AsNotSpecified e, AsReasonNotGiven e)
                => Proxy e
                -> FieldWidget t m r e Nest
nestWWorkflow _ = nestWorkflow $ \formFn ->
  let
    w1 = formFn $ FieldPiece (pure never) w1Buttons nestComplete1
    w1Buttons = do
      eNext <- button "Next"
      pure $ FieldAction w2 Validated Update <$ eNext

    w2 = formFn $ FieldPiece (pure never) w2Buttons nestComplete2
    w2Buttons = do
      eBack <- button "Back"
      eNext <- button "Next"
      pure $ leftmost [ FieldAction w1 Unvalidated Update <$ eBack
                      , FieldAction w3 Validated Update  <$ eNext
                      ]

    w3 = formFn $ WidgetPiece $ do
      text "Done!"
      eBack <- button "Back"
      pure $ FieldAction w2 Unvalidated Update <$ eBack
  in
    w1

data StorageTag a where
  NestTag :: StorageTag (Nest Maybe)

instance GEq StorageTag where
  geq NestTag NestTag = Just Refl

instance GCompare StorageTag where
  gcompare NestTag NestTag = GEQ

instance GKey StorageTag where
  toKey (This NestTag) = "nest"

  fromKey t =
    case t of
      "nest" -> Just (This NestTag)
      _ -> Nothing

  keys _ = [This NestTag]

instance ToJSONTag StorageTag Identity where
  toJSONTagged NestTag (Identity x) = toJSON x

instance FromJSONTag StorageTag Identity where
  parseJSONTagged NestTag x = Identity <$> parseJSON x

data TestError =
    ENotSpecified
  | EIntNotInt
  | EReasonNotGiven
  deriving (Eq, Ord, Show)

instance HasErrorMessage TestError where
  errorMessage ENotSpecified = "This is a required field"
  errorMessage EIntNotInt = "This needs to be an integer"
  errorMessage EReasonNotGiven = "This field is required if XYZ is False"

makePrisms ''TestError

instance AsNotSpecified TestError where
  _NotSpecified = _ENotSpecified

instance AsReasonNotGiven TestError where
  _ReasonNotGiven = _EReasonNotGiven

-- better demo
-- name / email (email input) / password (password input, 2 copies that must match) on page 1
-- address on page 2 (shipping / billing, with a "same as shipping" button)

nameW :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
       => FieldWidget t m r e (Wrap Text)
nameW =
  requiredText "Name"

newtype Email f =
  Email {
    unEmail :: Wrap Text f
  }

deriving instance Generic (Email Maybe)
instance ToJSON (Email Maybe) where
instance FromJSON (Email Maybe) where

instance NFunctor Email where
  nempty = Email $ Wrap Nothing
  ntraverse f (Email t) = Email <$> ntraverse f t

makeWrapped ''Email

emailW :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
       => FieldWidget t m r e Email
emailW = nest $ \child -> child $ Field const id _Wrapped $
  requiredText "Email"

newtype Password f =
  Password {
    unPassword :: Wrap Text f
  }

deriving instance Generic (Password Maybe)
instance ToJSON (Password Maybe) where
instance FromJSON (Password Maybe) where

instance NFunctor Password where
  nempty = Password $ Wrap Nothing
  ntraverse f (Password t) = Password <$> ntraverse f t

makeWrapped ''Password

passwordW :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
          => FieldWidget t m r e Password
passwordW = nest $ \child -> child $ Field const id _Wrapped $
  requiredText "Password"

data Address f =
  Address {
    _aStreet :: Wrap Text f
  , _aCity :: Wrap Text f
  , _aState :: Wrap Text f
  , _aPostcode :: Wrap Text f
  }

deriving instance Generic (Address Maybe)
instance ToJSON (Address Maybe) where
instance FromJSON (Address Maybe) where

instance NFunctor Address where
  nempty = Address nempty nempty nempty nempty
  ntraverse f (Address s c st p) = Address <$> ntraverse f s <*> ntraverse f c <*> ntraverse f st <*> ntraverse f p

makeLenses ''Address

addressW :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
         => FieldWidget t m r e Address
addressW = nest $ \child ->
  traverse_ (toFormRow . child)
    [ Field const (<> "-street") aStreet $
      requiredText "Street"
    , Field const (<> "-city") aCity $
      requiredText "City"
    , Field const (<> "-state") aState $
      requiredText "State"
    , Field const (<> "-postcode") aPostcode $
      requiredText "Postcode"
    ]

billingW :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
         => FieldWidget t m (Address Maybe, Wrap Bool Maybe) e Address
billingW =
  addressW

data Page1 f =
  Page1 {
    _p1Name :: Wrap Text f
  , _p1Email :: Email f
  , _p1Password :: Password f
  }

deriving instance Generic (Page1 Maybe)
instance ToJSON (Page1 Maybe) where
instance FromJSON (Page1 Maybe) where

instance NFunctor Page1 where
  nempty = Page1 nempty nempty nempty
  ntraverse f (Page1 n e p) = Page1 <$> ntraverse f n <*> ntraverse f e <*> ntraverse f p

makeLenses ''Page1

data Page2 f =
  Page2 {
    _p2Shipping :: Address f
  , _p2BillingSameAsShipping :: Wrap Bool f
  , _p2Billing :: Address f
  }

deriving instance Generic (Page2 Maybe)
instance ToJSON (Page2 Maybe) where
instance FromJSON (Page2 Maybe) where

instance NFunctor Page2 where
  nempty = Page2 nempty nempty nempty
  ntraverse f (Page2 s e b) = Page2 <$> ntraverse f s <*> ntraverse f e <*> ntraverse f b

makeLenses ''Page2

data DemoApp f =
  DemoApp {
    _daPage1 :: Page1 f
  , _daPage2 :: Page2 f
  }

deriving instance Generic (DemoApp Maybe)
instance ToJSON (DemoApp Maybe) where
instance FromJSON (DemoApp Maybe) where

instance NFunctor DemoApp where
  nempty = DemoApp nempty nempty
  ntraverse f (DemoApp p1 p2) = DemoApp <$> ntraverse f p1 <*> ntraverse f p2

makeLenses ''DemoApp

page1W :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
       => FieldWidget t m r e Page1
page1W = nest $ \child -> do
  el "h3" $ text "Page 1"
  traverse_ (toFormRow . child)
    [ Field const (<> "-name") p1Name
        nameW
    , Field const (<> "-email") p1Email
        emailW
    , Field const (<> "-password") p1Password
        passwordW
    ]

page2W :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
       => FieldWidget t m r e Page2
page2W = nest $ \child -> do
  el "h3" $ text "Page 2"
  traverse_ (toFormRow . child)
    [ Field const (<> "-shipping") p2Shipping
        addressW
    , Field const (<> "-same") p2BillingSameAsShipping
        boolW
    , Field (\_ p2 -> (view p2Shipping p2, view p2BillingSameAsShipping p2)) (<> "-billing") p2Billing
        billingW
    ]

demoAppW :: (MonadWidget t m, HasErrorMessage e, AsNotSpecified e)
         => Proxy e
         -> FieldWidget t m r e DemoApp
demoAppW _ =
  workflowList listButtons [
    workflowField . Field const (<> "-1") daPage1 $ page1W
  , workflowField . Field const (<> "-2") daPage2 $ page2W
  , workflowWidget $
      text "Done!"
  ]

deriving instance Show (Email Maybe)
deriving instance Show (Email Identity)

deriving instance Show (Password Maybe)
deriving instance Show (Password Identity)

deriving instance Show (Address Maybe)
deriving instance Show (Address Identity)

deriving instance Show (Page1 Maybe)
deriving instance Show (Page1 Identity)

deriving instance Show (Page2 Maybe)
deriving instance Show (Page2 Identity)

deriving instance Show (DemoApp Maybe)
deriving instance Show (DemoApp Identity)

runNest :: forall t m. MonadWidget t m => m ()
runNest = mdo
  -- _ <- runFieldWidgetWithStorage LocalStorage NestTag (nestWList (Proxy :: Proxy TestError)) (pure ()) mempty eV
  _ <- runFieldWidget (demoAppW (Proxy :: Proxy TestError)) (pure ()) mempty nempty never never
  pure ()

myTest :: IO ()
myTest = debugApp 8080 $ divClass "container" $ mdo
  runNest

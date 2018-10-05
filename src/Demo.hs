{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Demo where

import Reflex.Dom.Core

import Bootstrap

import Reflex.Dom.Validation

myTest :: IO ()
myTest = debugApp 8080 $ divClass "container" $ do
  go
  pure ()

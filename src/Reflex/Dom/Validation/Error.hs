{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Dom.Validation.Error where

import Data.Text (Text)

class HasErrorMessage e where
  errorMessage :: e -> Text

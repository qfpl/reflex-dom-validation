{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Dom.Validation.Requires where

data Requirement = Required | Optional
  deriving (Eq, Ord, Show, Read)

type family Requires (x :: Requirement) a where
  Requires 'Required a = a
  Requires 'Optional a = Maybe a

data SRequirement (x :: Requirement) a where
  SRequired :: a -> SRequirement 'Required a
  SOptional :: SRequirement 'Optional a

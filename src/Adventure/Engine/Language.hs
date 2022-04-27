{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Adventure.Engine.Language where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import GHC.Generics

newtype Verb = Verb { unVerb :: Text }
  deriving (Eq, Generic, Ord, Show)

deriving anyclass instance JSON.ToJSON Verb
deriving anyclass instance JSON.ToJSONKey Verb
deriving anyclass instance JSON.FromJSON Verb
deriving anyclass instance JSON.FromJSONKey Verb

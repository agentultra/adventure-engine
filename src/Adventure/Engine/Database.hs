{-# LANGUAGE DeriveGeneric #-}

module Adventure.Engine.Database where

import GHC.Generics

newtype EntityId a = EntityId { getEntityId :: Int }
  deriving (Eq, Generic, Ord, Show)

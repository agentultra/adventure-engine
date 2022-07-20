{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Adventure.Engine.Objects where

import Control.Lens
import qualified Data.Aeson as JSON
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import GHC.Generics

import Adventure.Engine.Database
import Adventure.Engine.Language

data Room
  = Room
  { _roomName        :: Text
  , _roomDescription :: Text
  , _roomObjects     :: Map Text (EntityId GameObject)
  , _roomExits       :: Map Text (EntityId Exit)
  , _roomDig         :: Either Text [(Text, Maybe (EntityId GameObject))]
  , _roomBackground  :: Maybe Text
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON Room
deriving instance JSON.FromJSON Room

data Item
  = Item
  { _itemSize'   :: Int
  , _itemWeight' :: Int
  , _itemVerbs   :: [(Verb, Verb)]
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON Item
deriving instance JSON.FromJSON Item

type ItemName = Text

item :: Int -> Int -> [(Verb, Verb)] -> Object
item size weight = ObjectItem . Item size weight

data ContainerLock
  = ContainerLock
  { _containerLockLock                :: Lock
  , _containerLockUnlockedDescription :: Text
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON ContainerLock
deriving instance JSON.FromJSON ContainerLock

data Container
  = Container
  { _containerSize  :: Int
  , _containerItems :: Map ItemName (EntityId GameObject)
  , _containerLock  :: Maybe ContainerLock
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON Container
deriving instance JSON.FromJSON Container

container :: Int -> Map ItemName (EntityId GameObject) -> Maybe ContainerLock -> Object
container size containerItems containerLock = ObjectContainer $ Container size containerItems containerLock

data Object
  = ObjectItem Item
  | ObjectContainer Container
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON Object
deriving instance JSON.FromJSON Object

data GameObject
  = GameObject
  { _gameObjectName        :: Text
  , _gameObjectDescription :: Text
  , _gameObjectObject      :: Object
  }
  deriving (Eq, Generic, Show)

describeGameObject :: GameObject -> Text
describeGameObject (GameObject _ desc (ObjectContainer (Container _ _ (Just (ContainerLock (Lock _ _ _ Locked) _))))) = desc
describeGameObject (GameObject _ _ (ObjectContainer (Container _ _ (Just (ContainerLock (Lock _ _ _ Unlocked) unlockedDesc))))) = unlockedDesc
describeGameObject (GameObject _ desc _) = desc

deriving instance JSON.ToJSON GameObject
deriving instance JSON.FromJSON GameObject

itemObject :: GameObject -> Maybe Item
itemObject (GameObject _ _ (ObjectItem item')) = pure item'
itemObject _                 = Nothing

data LockState = Locked | Unlocked
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON LockState
deriving instance JSON.FromJSON LockState

data Lock
  = Lock
  { _lockKeyItems   :: [EntityId GameObject]
  , _lockFailMsg    :: Text
  , _lockSuccessMsg :: Text
  , _lockState      :: LockState
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON Lock
deriving instance JSON.FromJSON Lock

data Exit
  = Exit
  { _exitName        :: Text
  , _exitDescription :: Text
  , _exitFrom        :: EntityId Room
  , _exitTo          :: EntityId Room
  , _exitLock        :: Maybe Lock
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON Exit
deriving instance JSON.FromJSON Exit

-- Orphan Instances

deriving anyclass instance JSON.ToJSON (EntityId Room)
deriving anyclass instance JSON.ToJSONKey (EntityId Room)
deriving anyclass instance JSON.ToJSON (EntityId Exit)
deriving anyclass instance JSON.ToJSONKey (EntityId Exit)
deriving anyclass instance JSON.ToJSON (EntityId GameObject)
deriving anyclass instance JSON.ToJSONKey (EntityId GameObject)
deriving anyclass instance JSON.ToJSON (EntityId Item)

deriving anyclass instance JSON.FromJSON (EntityId Room)
deriving anyclass instance JSON.FromJSONKey (EntityId Room)
deriving anyclass instance JSON.FromJSON (EntityId Exit)
deriving anyclass instance JSON.FromJSONKey (EntityId Exit)
deriving anyclass instance JSON.FromJSON (EntityId GameObject)
deriving anyclass instance JSON.FromJSONKey (EntityId GameObject)
deriving anyclass instance JSON.FromJSON (EntityId Item)

makeFields ''GameObject
makePrisms ''Object
makeFields ''Container
makeFields ''Item

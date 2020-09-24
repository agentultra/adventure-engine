{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adventure.Engine where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

newtype EntityId a = EntityId Int
  deriving (Eq, Ord, Show)

data Room
  = Room
  { _roomName        :: Text
  , _roomDescription :: Text
  , _roomItems       :: [EntityId Item]
  , _roomExits       :: [EntityId Exit]
  }
  deriving (Eq, Show)

data Item
  = Item
  { _itemName        :: Text
  , _itemDescription :: Text
  , _itemSize        :: Int
  , _itemWeight      :: Int
  }
  deriving (Eq, Show)

data Exit
  = Exit
  { _exitName        :: Text
  , _exitDescription :: Text
  , _exitFrom        :: EntityId Room
  , _exitTo          :: EntityId Room
  }
  deriving (Eq, Show)

data World
  = World
  { _worldRooms :: Map (EntityId Room) Room
  , _worldItems :: Map (EntityId Item) Item
  , _worldExits :: Map (EntityId Exit) Exit
  , _playerRoom :: EntityId Room
  }
  deriving (Eq, Show)

defaultRoom :: Room
defaultRoom = Room
  "The Front Porch"
  "There's a faded white picket fence in the yard and an old swing next to you."
  [EntityId 1, EntityId 2]
  []

shovel :: Item
shovel = Item "Shovel" "A rusted shovel with a wooden handle." 2 4

purse :: Item
purse = Item "Purse" "Weathered, old, leather purse." 1 1

defaultWorld :: World
defaultWorld = World
  (M.fromList [(EntityId 0, defaultRoom)])
  (M.fromList [(EntityId 1, shovel), (EntityId 2, purse)])
  M.empty
  (EntityId 0)

renderRoom :: Room -> [Item] -> [Exit] -> Text
renderRoom (Room name desc _ _) items exits = T.unlines
  [ name
  , "-----------"
  , desc
  , "----------"
  , "You see: " <> T.intercalate ", " (_itemName <$> items)
  , "Possible exits: " <> T.intercalate ", " (_exitName <$> exits)
  ]

-- Managing the World

data Command
  = Walk (EntityId Exit)
  | PickUp (EntityId Item)
  deriving (Eq, Show)

data GameError
  = RoomDoesNotExist (EntityId Room)
  | ItemDoesNotExist (EntityId Item)
  | ExitDoesNotExist (EntityId Exit)
  | SpaceWizard
  deriving (Eq, Show)

interact :: World -> Command -> Either GameError World
interact world = \case
  Walk exitId   -> walkTo world exitId
  PickUp _ -> undefined

walkTo :: World -> EntityId Exit -> Either GameError World
walkTo world@(World rooms _ exits playerRoom) exitId = do
  exit <- maybeToRight (ExitDoesNotExist exitId) $
    M.lookup exitId exits
  _ <- maybeToRight (RoomDoesNotExist (_exitFrom exit)) $
    M.lookup (_exitFrom exit) rooms
  _ <- maybeToRight (RoomDoesNotExist (_exitTo exit)) $
    M.lookup (_exitTo exit) rooms
  if _exitFrom exit /= playerRoom
    then Left SpaceWizard
    else pure $ world { _playerRoom = _exitTo exit }

render :: World -> Either GameError Text
render (World rooms items exits playerRoom) = do
  room <- maybeToRight (RoomDoesNotExist playerRoom) $
    M.lookup playerRoom rooms
  items' <- traverse getItem (_roomItems room)
  exits' <- traverse getExit (_roomExits room)
  pure $ renderRoom room items' exits'
  where
    getItem itemId  =
      case M.lookup itemId items of
       Nothing   -> Left $ ItemDoesNotExist itemId
       Just item -> Right item
    getExit exitId =
      case M.lookup exitId exits of
        Nothing   -> Left $ ExitDoesNotExist exitId
        Just exit -> Right exit

-- Utilities

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

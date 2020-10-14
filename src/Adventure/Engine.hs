{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adventure.Engine where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.Haskeline

newtype EntityId a = EntityId Int
  deriving (Eq, Ord, Show)

data Room
  = Room
  { _roomName        :: Text
  , _roomDescription :: Text
  , _roomItems       :: [EntityId Item]
  , _roomExits       :: Map Text (EntityId Exit)
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
  { _worldRooms      :: Map (EntityId Room) Room
  , _worldItems      :: Map (EntityId Item) Item
  , _worldExits      :: Map (EntityId Exit) Exit
  , _playerRoom      :: EntityId Room
  }
  deriving (Eq, Show)

frontPorch :: Room
frontPorch = Room
  "The Front Porch"
  "There's a faded white picket fence in the yard and an old swing next to you."
  [EntityId 1, EntityId 2]
  $ M.fromList [("Door", EntityId 3)]

mainHall :: Room
mainHall = Room
  "Main Hall"
  "The main hall of the house is plastered in yellowing wall paper."
  []
  $ M.fromList [("Door", EntityId 5)]

shovel :: Item
shovel = Item "Shovel" "A rusted shovel with a wooden handle." 2 4

purse :: Item
purse = Item "Purse" "Weathered, old, leather purse." 1 1

frontDoorOutside :: Exit
frontDoorOutside = Exit
  "Door"
  "It looks like it hasn't been opened in a long time."
  (EntityId 0)
  (EntityId 6)

frontDoorInside :: Exit
frontDoorInside = Exit
  "Door"
  "You came through here."
  (EntityId 6)
  (EntityId 0)

defaultWorld :: World
defaultWorld = World
  (M.fromList [(EntityId 0, frontPorch), (EntityId 6, mainHall)])
  (M.fromList [(EntityId 1, shovel), (EntityId 2, purse)])
  (M.fromList [(EntityId 3, frontDoorOutside), (EntityId 5, frontDoorInside)])
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
  | Look
  deriving (Eq, Show)

data GameError
  = RoomDoesNotExist (EntityId Room)
  | ItemDoesNotExist (EntityId Item)
  | ExitDoesNotExist (EntityId Exit)
  | SpaceWizard
  deriving (Eq, Show)

update :: World -> Command -> Either GameError World
update world = \case
  Walk exitId   -> walkTo world exitId
  PickUp _ -> undefined
  Look -> pure world

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
  exits' <- traverse getExit $ M.elems . _roomExits $ room
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

repl :: World -> IO ()
repl = runInputT defaultSettings . loop
  where
    loop :: World -> InputT IO ()
    loop world = do
      userInput <- getInputLine ">> "
      case userInput of
        Nothing -> pure ()
        Just "quit" -> do
          outputStrLn "Goodbye!"
          pure ()
        Just rawInput ->
          case parseInput . T.pack $ rawInput of
            Left err -> do
              outputStrLn $ show err
              loop world
            Right input ->
              case parseCommand input world of
                Left err -> do
                  outputStrLn $ show err
                  loop world
                Right cmd ->
                  case update world cmd of
                    Left err -> do
                      outputStrLn $ show err
                      loop world
                    Right world' ->
                      case render world' of
                        Left err -> do
                          outputStrLn $ show err
                          loop world'
                        Right rendered -> do
                          outputStrLn . T.unpack $ rendered
                          loop world'

newtype Verb = Verb Text
  deriving (Eq, Show)

walk :: Verb
walk = Verb "walk"

pickup :: Verb
pickup = Verb "pickup"

verbs :: [Text]
verbs = ["look", "walk", "pickup"]

data InputError
  = InvalidVerb
  | ParseFail
  | UnknownInput
  deriving (Eq, Show)

data Input
  = Input
  { _verb      :: Verb
  , _parameter :: Maybe Text
  }
  deriving (Eq, Show)

parseInput :: Text -> Either InputError Input
parseInput = mkInput . take 2 . T.split (==' ')
  where
    mkInput [] = Left ParseFail
    mkInput [x] = maybeToRight InvalidVerb $
        (\v -> Input (Verb v) Nothing) <$> find (== x) verbs
    mkInput (x:y:_) = maybeToRight InvalidVerb $
        (\v -> Input (Verb v) (Just y)) <$> find (== x) verbs

parseCommand :: Input -> World -> Either InputError Command
parseCommand input world = do
  let (Verb verb) = _verb input
  case verb of
    "walk" -> do
      room <- maybeToRight UnknownInput
        $ M.lookup (_playerRoom world) (_worldRooms world)
      let dest = case _parameter input of
                   Nothing -> Nothing
                   Just name -> M.lookup name (_roomExits room)
      case dest of
           Nothing -> Left UnknownInput
           Just exitId -> pure $ Walk exitId
    "look" -> pure $ Look
    _ -> Left InvalidVerb
{-
walk north
pickup shovel
-}

-- Utilities

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

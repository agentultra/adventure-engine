{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Adventure.Engine where

import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
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
  , _roomObjects     :: Map Text (EntityId GameObject)
  , _roomExits       :: Map Text (EntityId Exit)
  }
  deriving (Eq, Show)

data Item
  = Item
  { _itemSize'   :: Int
  , _itemWeight' :: Int
  }
  deriving (Eq, Show)

item :: Int -> Int -> Object
item size = ObjectItem . Item size

data Container
  = Container
  { _containerSize  :: Int
  , _containerItems :: Map ItemName (EntityId GameObject)
  }
  deriving (Eq, Show)

container :: Int -> Map ItemName (EntityId GameObject) -> Object
container size = ObjectContainer . Container size

data Object
  = ObjectItem Item
  | ObjectContainer Container
  deriving (Eq, Show)

data GameObject
  = GameObject
  { _gameObjectName        :: Text
  , _gameObjectDescription :: Text
  , _gameObjectObject      :: Object
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

data Scene
  = Scene
  { _sceneTitle       :: Text
  , _sceneDescription :: Text
  , _sceneObjects     :: [GameObject]
  , _sceneExits       :: [Exit]
  }
  deriving (Eq, Show)

data World
  = World
  { _worldRooms       :: Map (EntityId Room) Room
  , _worldObjects     :: Map (EntityId GameObject) GameObject
  , _worldExits       :: Map (EntityId Exit) Exit
  , _playerRoom       :: EntityId Room
  , _playerInventory  :: Map Text (EntityId GameObject)
  , _worldLogMessages :: [Text]
  }
  deriving (Eq, Show)

fetch :: Ord k => GameError -> k -> Map k v -> Either GameError v
fetch e key db = maybe (throwError e) pure $ M.lookup key db

currentRoom :: World -> Either GameError Room
currentRoom w = fetch SpaceWizard (_playerRoom w) (_worldRooms w)

exitCurrentRoom :: World -> Text -> Either GameError Exit
exitCurrentRoom w exitName = do
  room <- currentRoom w
  exitId <- fetch (ExitDoesNotExist' exitName) exitName (_roomExits room)
  fetch SpaceWizard exitId (_worldExits w)

getObjectInCurrentRoom :: World -> Text -> Either GameError (EntityId GameObject)
getObjectInCurrentRoom w objectName = do
  room <- currentRoom w
  fetch (ObjectNotInRoom objectName) objectName $ _roomObjects room

getObjectInInventory :: World -> Text -> Either GameError (EntityId GameObject)
getObjectInInventory w objectName =
  fetch (ObjectNotInInventory objectName) objectName $ _playerInventory w

getInventoryObjects :: World -> Either GameError [GameObject]
getInventoryObjects w = traverse (getObject w) $ M.elems (_playerInventory w)

getObject :: World -> EntityId GameObject -> Either GameError GameObject
getObject w objectId =
  fetch (ObjectDoesNotExist objectId) objectId $ _worldObjects w

-- TODO (james): a better way to show errors
data GameState
  = GameState
  { _gameStateVerbs       :: VerbAliasMap
  , _gameStateWorld       :: World
  , _gameStateScenes      :: [Scene]
  , _gameStateInputBuffer :: Text
  , _gameStateGameErrors  :: [GameError]
  }
  deriving (Eq)

newtype GameEngine m a = GameEngine { runEngine :: ExceptT GameError (StateT GameState m) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadState GameState
    , MonadError GameError
    )

frontPorch :: Room
frontPorch = Room
  "The Front Porch"
  "There's a faded white picket fence in the yard and an old swing next to you."
  (M.fromList [("shovel", EntityId 1), ("purse", EntityId 2)])
  (M.fromList [("front door", EntityId 3)])

mainHall :: Room
mainHall = Room
  "Main Hall"
  "The main hall of the house is plastered in yellowing wall paper."
  (M.fromList [("brolly bucket", EntityId 8)])
  (M.fromList [("door", EntityId 5)])

shovel :: GameObject
shovel = GameObject "Shovel" "A rusted shovel with a wooden handle." $ item 2 4

purse :: GameObject
purse = GameObject "Purse" "Weathered, old, leather purse." $ item 1 1

magicBrolly :: GameObject
magicBrolly
  = GameObject "Brolly" "A plain, black brolly. The head of a dragon is carved into the wooden handle."
  $ item 1 1

bucket :: GameObject
bucket
  = GameObject "Brolly Bucket" "A rusting, iron bucket. There may be some brollies in it."
  $ container 4 (M.fromList [("brolly", EntityId 7)])

frontDoorOutside :: Exit
frontDoorOutside = Exit
  "Front Door"
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
  (M.fromList
   [ (EntityId 1, shovel)
   , (EntityId 2, purse)
   , (EntityId 7, magicBrolly)
   , (EntityId 8, bucket)
   ])
  (M.fromList [(EntityId 3, frontDoorOutside), (EntityId 5, frontDoorInside)])
  (EntityId 0)
  M.empty
  mempty

-- Managing the World

type ItemName = Text

type VerbAliasMap = Map Verb Verb

defaultVerbAliasMap :: VerbAliasMap
defaultVerbAliasMap
  = M.fromList
  [ (Verb "l", Verb "look")
  , (Verb "look", Verb "look")
  , (Verb "w", Verb "walk")
  , (Verb "walk", Verb "walk")
  , (Verb "p", Verb "pickup")
  , (Verb "pickup", Verb "pickup")
  , (Verb "d", Verb "drop")
  , (Verb "drop", Verb "drop")
  , (Verb "e", Verb "examine")
  , (Verb "examine", Verb "examine")
  , (Verb "t", Verb "take")
  , (Verb "take", Verb "take")
  ]

getVerb :: VerbAliasMap -> Verb -> Either GameError Verb
getVerb verbAliasMap v =
  maybeToRight (UnrecognizedVerb v) $ M.lookup v verbAliasMap

parse :: Text -> Either GameError (Verb, [Text])
parse = parseCmd . T.words
  where
    parseCmd [] = Left MissingCommand
    parseCmd (x:xs) = pure (Verb x, xs)

defaultGameState :: GameState
defaultGameState
  = GameState
  defaultVerbAliasMap
  defaultWorld
  []
  ""
  []

initialGameState :: Either GameError GameState
initialGameState = do
  initialScene <- render defaultWorld
  pure $ defaultGameState { _gameStateScenes = [initialScene] }

handle' :: GameState -> World -> Text -> Either GameError World
handle' game world input = do
  (v, args) <- first (const MissingCommand) . parse $ input
  verb <- first (const $ UnrecognizedVerb v)
    $ getVerb (_gameStateVerbs game) v
  --_commandWith cmd world args
  handleVerb verb world args

data GameError
  = RoomDoesNotExist (EntityId Room)
  | ObjectDoesNotExist (EntityId GameObject)
  | ObjectNotInRoom Text
  | ObjectNotInInventory Text
  | ObjectNotInContainer Text
  | InvalidObjectParameter Text
  | ExitDoesNotExist (EntityId Exit)
  | ExitDoesNotExist' Text
  | MissingCommand
  | UnrecognizedVerb Verb
  | MissingParameter Text
  | SpaceWizard
  deriving (Eq, Show)

instance Exception GameError

gameErrorText :: GameError -> Text
gameErrorText = \case
  RoomDoesNotExist _         -> "That room does not exist."
  ObjectDoesNotExist _       -> "That object does not exist."
  ObjectNotInRoom txt        -> "I don't see '" <> txt <> "' here."
  ObjectNotInInventory txt   -> "You are not holding, '" <> txt <> "'."
  ObjectNotInContainer txt   -> "I don't see '" <> txt <> "' in there."
  InvalidObjectParameter txt -> "You can't really do that with, '" <> txt <> "'."
  ExitDoesNotExist _         -> "I don't see that exit here."
  ExitDoesNotExist' txt      -> "I don't see an exit called, '" <> txt <> "' here."
  MissingCommand             -> "Did you mean to give me a command?"
  UnrecognizedVerb v         -> "I don't know what you mean by, '" <> (unVerb v) <> "'."
  MissingParameter txt       -> "I'm missing more information about, '" <> txt <> "'."
  SpaceWizard                -> "SPACE WIZARD!!!!"

type CommandHandler = World -> [Text] -> Either GameError World

handleVerb :: Verb -> World -> [Text] -> Either GameError World
handleVerb (Verb "walk") w args = handleWalk w args
handleVerb (Verb "pickup") w args = handlePickup w args
handleVerb (Verb "drop") w args = handleDrop w args
handleVerb (Verb "look") w args = handleLook w args
handleVerb (Verb "examine") w args = handleExamine w args
handleVerb (Verb "take") w args = handleTake w args
handleVerb v _ _ = Left $ UnrecognizedVerb v

handleWalk :: CommandHandler
handleWalk _ [] = Left $ MissingParameter "missing destination"
handleWalk world args = do
  let rooms      = _worldRooms world
      playerRoom = _playerRoom world
      exitName   = keyArg args

  exit <- exitCurrentRoom world exitName
  _ <- maybeToRight (RoomDoesNotExist (_exitFrom exit)) $
    M.lookup (_exitFrom exit) rooms
  _ <- maybeToRight (RoomDoesNotExist (_exitTo exit)) $
    M.lookup (_exitTo exit) rooms
  if _exitFrom exit /= playerRoom
    then Left SpaceWizard
    else pure $ world { _playerRoom = _exitTo exit }

handlePickup :: CommandHandler
handlePickup _ [] = Left $ MissingParameter "pickup what?"
handlePickup world args = do
  let objectName = keyArg args
      playerRoom = _playerRoom world
      playerInv  = _playerInventory world
      rooms      = _worldRooms world
      objects    = _worldObjects world

  objectId <- getObjectInCurrentRoom world objectName
  _ <- maybeToRight SpaceWizard $
    M.lookup playerRoom rooms
  _ <- maybeToRight (ObjectDoesNotExist objectId) $
    M.lookup objectId objects
  pure $ world
    { _worldRooms = M.adjust (removeItem objectName) playerRoom rooms
    , _playerInventory = M.insert objectName objectId playerInv
    }
  where
    removeItem objectName r = r
      { _roomObjects = M.update (const Nothing) objectName (_roomObjects r)
      }

handleLook :: CommandHandler
handleLook w [] = pure w
handleLook w args =
  case peek "in" args of
    Just args' -> lookInContainer w . keyArg $ args'
    Nothing -> do
      let exitName = keyArg args
      exit <- exitCurrentRoom w exitName
      pure $ w { _worldLogMessages = _worldLogMessages w <> [_exitDescription exit] }

lookInContainer :: World -> Text -> Either GameError World
lookInContainer w containerName = do
  containerId <- getObjectInCurrentRoom w containerName
  container'  <- getObject w containerId
  case _gameObjectObject container' of
    ObjectItem _ -> Left $ InvalidObjectParameter containerName
    ObjectContainer cont -> do
      items <- traverse (getObject w) $ M.elems (_containerItems cont)
      let itemNames = T.intercalate ", " . map _gameObjectName $ items
      pure $ w
        { _worldLogMessages = _worldLogMessages w
          <> ["Inside the " <> containerName <> " you see: " <> itemNames]
        }

handleDrop :: CommandHandler
handleDrop _ [] = Left $ MissingParameter "drop what?"
handleDrop world args = do
  let playerPos = _playerRoom world
      playerInv = _playerInventory world
      rooms     = _worldRooms world
      objectName = keyArg args

  objectId <- maybeToRight (ObjectNotInInventory objectName) $
    M.lookup objectName (_playerInventory world)

  pure $ world
    { _worldRooms = M.adjust (addObject objectName objectId) playerPos rooms
    , _playerInventory = M.delete objectName playerInv
    }
  where
    addObject objectName objectId r
      = r { _roomObjects = M.insert objectName objectId (_roomObjects r) }

handleExamine :: CommandHandler
handleExamine _ [] = Left $ MissingParameter "examine what?"
handleExamine world args = do
  let objectName = maybe (keyArg args) keyArg $ peek "my" args

  case peek "my" args of
    Just _ ->
      examineInInventory world objectName
    Nothing    ->
      examineInRoom world objectName

examineInInventory :: World -> ItemName -> Either GameError World
examineInInventory world objectName = do
  let msgs  = _worldLogMessages world

  objectId <- getObjectInInventory world objectName
  object <- getObject world objectId

  pure $ world { _worldLogMessages = msgs <> [_gameObjectDescription object] }

examineInRoom :: World -> ItemName -> Either GameError World
examineInRoom world objectName = do
  let msgs = _worldLogMessages world

  objectId <- getObjectInCurrentRoom world objectName
  object   <- getObject world objectId

  pure $ world {_worldLogMessages = msgs <> [_gameObjectDescription object]}

handleTake :: CommandHandler
handleTake w args = case splitAtWord "from" args of
  Nothing       -> Left $ MissingParameter "take what from what?"
  Just (xs, ys) -> do
    let containerName = keyArg ys
        itemName      = keyArg xs
        playerInv     = _playerInventory w
        objects       = _worldObjects w

    objectId <- getObjectInCurrentRoom w containerName
    object   <- getObject w objectId
    case _gameObjectObject object of
      ObjectItem _ -> Left SpaceWizard
      ObjectContainer container' -> do
        itemId <- maybeToRight (ObjectNotInContainer itemName) $
          M.lookup itemName (_containerItems container')
        let object' = object { _gameObjectObject = ObjectContainer container' { _containerItems = M.delete itemName (_containerItems container') } }

        pure $ w
          { _worldObjects = M.insert objectId object' objects
          , _playerInventory = M.insert itemName itemId playerInv
          }

render :: World -> Either GameError Scene
render w@(World _ _ exits _ playerInv _) = do
  room       <- currentRoom w
  objects'   <- traverse (getObject w) $ M.elems (_roomObjects room)
  exits'     <- traverse getExit $ M.elems . _roomExits $ room
  pure $ Scene
    { _sceneTitle = _roomName room
    , _sceneDescription = _roomDescription room
    , _sceneObjects = objects'
    , _sceneExits = exits'
    }
  where
    getExit exitId =
      case M.lookup exitId exits of
        Nothing   -> Left $ ExitDoesNotExist exitId
        Just exit -> Right exit

newtype Verb = Verb { unVerb :: Text }
  deriving (Eq, Ord, Show)

-- Utilities

-- | if the first word matches the head of the list return the rest of
-- the list otherwise nothing.
peek :: Text -> [Text] -> Maybe [Text]
peek _ [] = Nothing
peek word (word':words')
  | word == word' = Just words'
  | otherwise     = Nothing

-- | If we find the matching word in the input text and more than one
-- word on each side of it, return the pair of lists otherwise
-- nothing.
splitAtWord :: Text -> [Text] -> Maybe ([Text], [Text])
splitAtWord w ws = case span (/= w) ws of
  ([], _) -> Nothing
  (xs, ys) | length ys > 1 -> Just (xs, tail ys)
           | otherwise     -> Nothing

keyArg :: [Text] -> Text
keyArg = T.unwords . map T.toLower

maybeElem :: (Foldable t, Eq a) => a -> t a -> Maybe a
maybeElem x xs =
  if x `elem` xs
  then Just x
  else Nothing

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight y Nothing  = Left y

eitherToInput :: MonadError e (InputT m) => Either e a -> InputT m a
eitherToInput (Left err)     = throwError err
eitherToInput (Right result) = pure result

makeFields ''GameState
makeFields ''World

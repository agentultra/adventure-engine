{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Adventure.Engine where

import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics
import System.Console.Haskeline
import System.Directory
import System.FilePath

import Adventure.List.Utils

newtype EntityId a = EntityId { getEntityId :: Int }
  deriving (Eq, Generic, Ord, Show)

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

data Room
  = Room
  { _roomName        :: Text
  , _roomDescription :: Text
  , _roomObjects     :: Map Text (EntityId GameObject)
  , _roomExits       :: Map Text (EntityId Exit)
  , _roomDig         :: Either Text [(Text, Maybe (EntityId GameObject))]
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

item :: Int -> Int -> [(Verb, Verb)] -> Object
item size weight = ObjectItem . Item size weight

data Container
  = Container
  { _containerSize  :: Int
  , _containerItems :: Map ItemName (EntityId GameObject)
  , _containerLock  :: Maybe Lock
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON Container
deriving instance JSON.FromJSON Container

container :: Int -> Map ItemName (EntityId GameObject) -> Maybe Lock -> Object
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

deriving instance JSON.ToJSON GameObject
deriving instance JSON.FromJSON GameObject

itemObject :: GameObject -> Maybe Item
itemObject (GameObject _ _ (ObjectItem item')) = pure item'
itemObject _                 = Nothing

itemObjectVerbAliasMap :: GameObject -> Maybe (Map Verb Verb)
itemObjectVerbAliasMap (GameObject _ _ (ObjectItem item'))
  = pure . M.fromList . _itemVerbs $ item'
itemObjectVerbAliasMap _ = Nothing

data ExitLock
  = ExitLock
  { _exitLockKeyItems    :: [EntityId Item]
  , _exitLockFailText    :: Text
  , _exitLockSuccessText :: Text
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON ExitLock
deriving instance JSON.FromJSON ExitLock

data LockState = Locked | Unlocked
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON LockState
deriving instance JSON.FromJSON LockState

data Lock
  = Lock
  { _lockKeyItems       :: [EntityId Item]
  , _lockKeyErrorMsgs   :: Map Verb Text
  , _lockKeySuccessMsgs :: Map Verb Text
  , _lockState          :: LockState
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
  , _exitLock        :: Maybe ExitLock
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON Exit
deriving instance JSON.FromJSON Exit

data Scene
  = Scene
  { _sceneTitle       :: Text
  , _sceneDescription :: Text
  , _sceneObjects     :: [GameObject]
  , _sceneExits       :: [Exit]
  , _sceneMessages    :: [Text]
  }
  deriving (Eq, Generic, Show)

deriving instance JSON.ToJSON Scene
deriving instance JSON.FromJSON Scene

data World
  = World
  { _worldRooms       :: Map (EntityId Room) Room
  , _worldObjects     :: Map (EntityId GameObject) GameObject
  , _worldExits       :: Map (EntityId Exit) Exit
  , _playerRoom       :: EntityId Room
  , _playerInventory  :: Map Text (EntityId GameObject)
  , _playerVerbs      :: VerbAliasMap
  , _worldLogMessages :: [Text]
  }
  deriving (Eq, Show)

instance JSON.ToJSON World where
  toJSON (World rooms objects exits pRoom pInv pVerbs _) =
    JSON.object
    [ "rooms"           .= rooms
    , "objects"         .= objects
    , "exits"           .= exits
    , "playerRoom"      .= pRoom
    , "playerInventory" .= pInv
    , "playerVerbs"     .= pVerbs
    ]

instance JSON.FromJSON World where
  parseJSON = JSON.withObject "World" $ \v ->
    World
    <$> v .: "rooms"
    <*> v .: "objects"
    <*> v .: "exits"
    <*> v .: "playerRoom"
    <*> v .: "playerInventory"
    <*> v .: "playerVerbs"
    <*> pure mempty

fetch :: (Monad m, Ord k) => GameError -> k -> Map k v -> ExceptT GameError m v
fetch e key db = maybe (throwError e) pure $ M.lookup key db

currentRoom :: Monad m => World -> ExceptT GameError m Room
currentRoom w = fetch SpaceWizard (_playerRoom w) (_worldRooms w)

exitCurrentRoom :: Monad m => World -> Text -> ExceptT GameError m Exit
exitCurrentRoom w exitName = do
  room <- currentRoom w
  exitId <- fetch (ExitDoesNotExist' exitName) exitName (_roomExits room)
  fetch SpaceWizard exitId (_worldExits w)

getObjectInCurrentRoom
  :: Monad m
  => World
  -> Text
  -> ExceptT GameError m (EntityId GameObject)
getObjectInCurrentRoom w objectName = do
  room <- currentRoom w
  fetch (ObjectNotInRoom objectName) objectName $ _roomObjects room

updateCurrentRoom :: Monad m => World -> Room -> ExceptT GameError m World
updateCurrentRoom w room =
  let roomMap = _worldRooms w
  in pure $ w
     { _worldRooms = M.update (const . Just $ room) (_playerRoom w) roomMap
     }

getObjectInInventory
  :: Monad m
  => World
  -> Text
  -> ExceptT GameError m (EntityId GameObject)
getObjectInInventory w objectName =
  fetch (ObjectNotInInventory objectName) objectName $ _playerInventory w

getInventoryObjects :: Monad m => World -> ExceptT GameError m [GameObject]
getInventoryObjects w = traverse (getObject w) $ M.elems (_playerInventory w)

getObject :: Monad m => World -> EntityId GameObject -> ExceptT GameError m GameObject
getObject w objectId =
  fetch (ObjectDoesNotExist objectId) objectId $ _worldObjects w

-- TODO (james): Might replace this with accessors into some kind of player state record
getPlayerInventory :: GameState -> [GameObject]
getPlayerInventory gameState =
  case runExcept $ getInventoryObjects $ _gameStateWorld gameState of
    Left err -> error $ "Error getting inventory: " ++ show err
    Right objects -> objects

-- TODO (james): a better way to show errors
data GameState
  = GameState
  { _gameStateWorld       :: World
  , _gameStateScenes      :: [Scene]
  , _gameStateInputBuffer :: Text
  , _gameStateGameErrors  :: [Text]
  }
  deriving (Eq, Show)

instance JSON.ToJSON GameState where
  toJSON (GameState world scenes _ _) =
    JSON.object
    [ "world" .= world
    , "scenes" .= scenes
    ]

instance JSON.FromJSON GameState where
  parseJSON = JSON.withObject "GameState" $ \v ->
    GameState
    <$> v .: "world"
    <*> v .: "scenes"
    <*> pure mempty
    <*> pure mempty

newtype GameEngine m a = GameEngine { runEngine :: ExceptT GameError (StateT GameState m) a }
  deriving newtype
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
  (M.fromList [("shovel", EntityId 1), ("purse", EntityId 2), ("fire", EntityId 10)])
  (M.fromList [("front door", EntityId 3)])
  (Right [ ( "You get a little hot digging but find something..."
           , Just $ EntityId 9
           )
         ]
  )

mainHall :: Room
mainHall = Room
  "Main Hall"
  "The main hall of the house is plastered in yellowing wall paper."
  (M.fromList [("brolly bucket", EntityId 8)])
  (M.fromList [("door", EntityId 5)])
  (Left "You can't do that here...")

shovel :: GameObject
shovel = GameObject "Shovel" "A rusted shovel with a wooden handle."
  $ item 2 4 [(Verb "dig", Verb "dig")]

purse :: GameObject
purse = GameObject "Purse" "Weathered, old, leather purse." $ item 1 1 []

magicBrolly :: GameObject
magicBrolly
  = GameObject "Brolly" "A plain, black brolly. The head of a dragon is carved into the wooden handle."
  $ item 1 1 []

bucket :: GameObject
bucket
  = GameObject "Brolly Bucket" "A rusting, iron bucket. There may be some brollies in it."
  $ container 4 (M.fromList [("brolly", EntityId 7)]) Nothing

coin :: GameObject
coin
 = GameObject "A Wierd Coin" "A coin with a face that is hard to discern."
  $ item 1 1 []

fireLock :: Lock
fireLock = Lock
  []
  (M.fromList [(Verb "look", "It hurts your eyes to get too close but there is something dark in there, if only you could put out the flames.")])
  (M.fromList [(Verb "look", "Success! You have put the fire out...")])
  Unlocked

fire :: GameObject
fire = GameObject "A Roaring Fire" "A roaring fire fed by some unknown source of fuel. If you look closely there is some object in the flames."
  $ container 4 M.empty (Just fireLock)

frontDoorOutside :: Exit
frontDoorOutside = Exit
  "Front Door"
  "It looks like it hasn't been opened in a long time."
  (EntityId 0)
  (EntityId 6)
  (Just . ExitLock [EntityId 1] "You find it won't budge..." $ "You knock in the door with the shovel!")

frontDoorInside :: Exit
frontDoorInside = Exit
  "Door"
  "You came through here."
  (EntityId 6)
  (EntityId 0)
  Nothing

defaultWorld :: World
defaultWorld = World
  (M.fromList [(EntityId 0, frontPorch), (EntityId 6, mainHall)])
  (M.fromList
   [ (EntityId 1, shovel)
   , (EntityId 2, purse)
   , (EntityId 7, magicBrolly)
   , (EntityId 8, bucket)
   , (EntityId 9, coin)
   , (EntityId 10, fire)
   ])
  (M.fromList [(EntityId 3, frontDoorOutside), (EntityId 5, frontDoorInside)])
  (EntityId 0)
  M.empty
  defaultVerbAliasMap
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

getVerb :: Monad m => VerbAliasMap -> Verb -> ExceptT GameError m Verb
getVerb verbAliasMap v =
  maybeThrow (UnrecognizedVerb v) $ M.lookup v verbAliasMap

parse :: Monad m => Text -> ExceptT GameError m (Verb, [Text])
parse = parseCmd . T.words
  where
    parseCmd :: Monad m => [Text] -> ExceptT GameError m (Verb, [Text])
    parseCmd [] = throwError MissingCommand
    parseCmd (x:xs) = pure (Verb x, xs)

defaultGameState :: GameState
defaultGameState
  = GameState
  defaultWorld
  []
  ""
  []

initialGameState :: Either GameError GameState
initialGameState = do
  initialScene <- runExcept $ render defaultWorld
  pure $ defaultGameState { _gameStateScenes = [initialScene] }

handle' :: Monad m => GameState -> World -> Text -> ExceptT GameError m World
handle' _ world input = do
  (v, args) <- parse input
  verb <- getVerb (_playerVerbs world) v
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
  | SaveFileError Text
  | SaveLoadError Text
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
  UnrecognizedVerb v         -> "I don't know what you mean by, '" <> unVerb v <> "'."
  MissingParameter txt       -> "I'm missing more information about, '" <> txt <> "'."
  SaveLoadError txt          -> "Error loading save file: " <> txt
  SaveFileError txt          -> "Error saving file: " <> txt
  SpaceWizard                -> "SPACE WIZARD!!!!"

handleVerb :: Monad m => Verb -> World -> [Text] -> ExceptT GameError m World
handleVerb (Verb "walk") w args = handleWalk w args
handleVerb (Verb "pickup") w args = handlePickup w args
handleVerb (Verb "drop") w args = handleDrop w args
handleVerb (Verb "look") w args = handleLook w args
handleVerb (Verb "examine") w args = handleExamine w args
handleVerb (Verb "take") w args = handleTake w args
handleVerb (Verb "dig") w args = handleDig w args
handleVerb v _ _ = throwError $ UnrecognizedVerb v

handleWalk :: Monad m => World -> [Text] -> ExceptT GameError m World
handleWalk _ [] = throwError $ MissingParameter "missing destination"
handleWalk world args = do
  let rooms      = _worldRooms world
      playerRoom = _playerRoom world
      exitName   = keyArg args
      messages   = _worldLogMessages world

  exit <- exitCurrentRoom world exitName
  _ <- maybeThrow (RoomDoesNotExist (_exitFrom exit)) $
    M.lookup (_exitFrom exit) rooms
  _ <- maybeThrow (RoomDoesNotExist (_exitTo exit)) $
    M.lookup (_exitTo exit) rooms
  case _exitLock exit of
    Nothing -> movePlayer exit playerRoom world
    Just (ExitLock exitKeyItems errText successText)
      | playerHasKeyItems (M.elems . _playerInventory $ world) exitKeyItems ->
        let world' = world { _worldLogMessages = successText : messages }
        in movePlayer exit playerRoom world'
      | otherwise -> pure $ world { _worldLogMessages = errText : messages }
  where
    movePlayer :: Monad m => Exit -> EntityId Room -> World -> ExceptT GameError m World
    movePlayer ext rm wrld
      | _exitFrom ext == rm = pure $ wrld { _playerRoom = _exitTo ext }
      | otherwise           = throwError SpaceWizard
    playerHasKeyItems :: [EntityId GameObject] -> [EntityId Item] -> Bool
    playerHasKeyItems playerInventory exitKeyItems =
      let exitEntityIds = map getEntityId exitKeyItems
      in exitEntityIds `intersect` map getEntityId playerInventory == exitEntityIds

handlePickup :: Monad m => World -> [Text] -> ExceptT GameError m World
handlePickup _ [] = throwError $ MissingParameter "pickup what?"
handlePickup world args = do
  let objectName = keyArg args
      playerRoom = _playerRoom world
      playerInv  = _playerInventory world
      rooms      = _worldRooms world
      objects    = _worldObjects world

  objectId <- getObjectInCurrentRoom world objectName
  _ <- maybeThrow SpaceWizard $
    M.lookup playerRoom rooms
  object <- maybeThrow (ObjectDoesNotExist objectId) $
    M.lookup objectId objects
  -- TODO (james): add support for container verbiage
  itemVerbs <- maybeThrow SpaceWizard . itemObjectVerbAliasMap $ object
  pure $ world
    { _worldRooms = M.adjust (removeItem objectName) playerRoom rooms
    , _playerInventory = M.insert objectName objectId playerInv
    , _playerVerbs = M.union (_playerVerbs world) itemVerbs
    }
  where
    removeItem objectName r = r
      { _roomObjects = M.update (const Nothing) objectName (_roomObjects r)
      }

handleLook :: Monad m => World -> [Text] -> ExceptT GameError m World
handleLook w [] = pure w
handleLook w args =
  case peek "in" args of
    Just args' -> lookInContainer w . keyArg $ args'
    Nothing -> do
      let exitName = keyArg args
      exit <- exitCurrentRoom w exitName
      pure $ w { _worldLogMessages = _worldLogMessages w <> [_exitDescription exit] }

lookInContainer :: Monad m => World -> Text -> ExceptT GameError m World
lookInContainer w containerName = do
  containerId <- getObjectInCurrentRoom w containerName
  container'  <- getObject w containerId
  case _gameObjectObject container' of
    ObjectItem _ -> throwError $ InvalidObjectParameter containerName
    ObjectContainer cont ->
      case _containerLock cont of
        Nothing -> doLookInContainer cont
        Just lck ->
          case _lockState lck of
            Locked -> do
              lockMsg <- maybeThrow SpaceWizard $ M.lookup (Verb "look") (_lockKeyErrorMsgs lck)
              pure $ w
                { _worldLogMessages = _worldLogMessages w <> [ lockMsg ]
                }
            Unlocked -> doLookInContainer cont
  where
    doLookInContainer cont = do
      items <- traverse (getObject w) $ M.elems (_containerItems cont)
      let itemNames = T.intercalate ", " . map _gameObjectName $ items
      pure $ w
        { _worldLogMessages = _worldLogMessages w
          <> ["Inside the " <> containerName <> " you see: " <> itemNames]
        }

handleDrop :: Monad m => World -> [Text] -> ExceptT GameError m World
handleDrop _ [] = throwError $ MissingParameter "drop what?"
handleDrop world args = do
  let playerPos   = _playerRoom world
      playerInv   = _playerInventory world
      playerVerbs = _playerVerbs world
      rooms       = _worldRooms world
      objectName  = keyArg args

  objectId <- maybeThrow (ObjectNotInInventory objectName) $
    M.lookup objectName (_playerInventory world)
  object <- maybeThrow (ObjectNotInInventory objectName) $
    M.lookup objectId (_worldObjects world)

  let objectVerbs = fromMaybe M.empty . itemObjectVerbAliasMap $ object

  pure $ world
    { _worldRooms = M.adjust (addObject objectName objectId) playerPos rooms
    , _playerInventory = M.delete objectName playerInv
    , _playerVerbs = M.difference playerVerbs objectVerbs
    }
  where
    addObject objectName objectId r
      = r { _roomObjects = M.insert objectName objectId (_roomObjects r) }

handleExamine :: Monad m => World -> [Text] -> ExceptT GameError m World
handleExamine _ [] = throwError $ MissingParameter "examine what?"
handleExamine world args = do
  let objectName = maybe (keyArg args) keyArg $ peek "my" args

  case peek "my" args of
    Just _ ->
      examineInInventory world objectName
    Nothing    ->
      examineInRoom world objectName

examineInInventory :: Monad m => World -> ItemName -> ExceptT GameError m World
examineInInventory world objectName = do
  let msgs  = _worldLogMessages world

  objectId <- getObjectInInventory world objectName
  object <- getObject world objectId

  pure $ world { _worldLogMessages = msgs <> [_gameObjectDescription object] }

examineInRoom :: Monad m => World -> ItemName -> ExceptT GameError m World
examineInRoom world objectName = do
  let msgs = _worldLogMessages world

  objectId <- getObjectInCurrentRoom world objectName
  object   <- getObject world objectId

  pure $ world {_worldLogMessages = msgs <> [_gameObjectDescription object]}

handleTake :: Monad m => World -> [Text] -> ExceptT GameError m World
handleTake w args = case splitAtWord "from" args of
  Nothing       -> throwError $ MissingParameter "take what from what?"
  Just (xs, ys) -> do
    let containerName = keyArg ys
        itemName      = keyArg xs
        playerInv     = _playerInventory w
        objects       = _worldObjects w

    objectId <- getObjectInCurrentRoom w containerName
    object   <- getObject w objectId
    case _gameObjectObject object of
      ObjectItem _ -> throwError SpaceWizard
      ObjectContainer container' -> do
        itemId <- maybeThrow (ObjectNotInContainer itemName) $
          M.lookup itemName (_containerItems container')
        let object' = object { _gameObjectObject = ObjectContainer container' { _containerItems = M.delete itemName (_containerItems container') } }

        pure $ w
          { _worldObjects = M.insert objectId object' objects
          , _playerInventory = M.insert itemName itemId playerInv
          }

handleDig :: Monad m => World -> [Text] -> ExceptT GameError m World
handleDig world _ = do
  let msgs = _worldLogMessages world
      playerInv = _playerInventory world
  room <- currentRoom world
  case _roomDig room of
    Left failMsg -> pure $ world { _worldLogMessages = msgs <> [failMsg] }
    Right [] ->
      pure $ world { _worldLogMessages = msgs <> ["You dig but nothing seems to come of it..."] }
    Right ((successMsg, mItem):rest) -> do
      let room' = room { _roomDig = rest <$ _roomDig room }
      world'' <- updateCurrentRoom world room'
      case mItem of
        Nothing ->
          pure $ world'' { _worldLogMessages = msgs <> [successMsg] }
        Just itemId -> do
          object <- getObject world itemId
          pure $ world''
            { _playerInventory = M.insert (_gameObjectName object) itemId playerInv
            , _worldLogMessages = msgs <> [successMsg]
            }

render :: Monad m => World -> ExceptT GameError m Scene
render w@(World _ _ exits _ playerInv _ messages) = do
  room       <- currentRoom w
  objects'   <- traverse (getObject w) $ M.elems (_roomObjects room)
  exits'     <- traverse getExit $ M.elems . _roomExits $ room
  pure $ Scene
    { _sceneTitle = _roomName room
    , _sceneDescription = _roomDescription room
    , _sceneObjects = objects'
    , _sceneExits = exits'
    , _sceneMessages = messages
    }
  where
    getExit exitId =
      case M.lookup exitId exits of
        Nothing   -> throwError $ ExitDoesNotExist exitId
        Just exit -> pure exit

newtype Verb = Verb { unVerb :: Text }
  deriving (Eq, Generic, Ord, Show)

deriving anyclass instance JSON.ToJSON Verb
deriving anyclass instance JSON.ToJSONKey Verb
deriving anyclass instance JSON.FromJSON Verb
deriving anyclass instance JSON.FromJSONKey Verb

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

maybeThrow :: Monad m => b -> Maybe a -> ExceptT b m a
maybeThrow _ (Just x) = pure x
maybeThrow y Nothing  = throwError y

eitherToInput :: MonadError e (InputT m) => Either e a -> InputT m a
eitherToInput (Left err)     = throwError err
eitherToInput (Right result) = pure result

makeFields ''GameState
makeFields ''World

updateGame :: GameState -> GameState
updateGame g@(GameState w rvs input errors) =
  case runExcept $ handle' g w input of
    Left err -> updateGameErrors g err
    Right world' ->
      case runExcept $ render world' of
        Left renderErr -> updateGameErrors g renderErr
        Right rendered ->
          g & scenes .~ rendered : rvs
            & world .~ world' { _worldLogMessages = [] }
            & inputBuffer .~ ""
  where
    updateGameErrors :: GameState -> GameError -> GameState
    updateGameErrors (GameState _ _ _ errs) e
      | length errs < 3 = g & gameErrors .~ gameErrorText e : errs
      | otherwise = g & gameErrors .~ prepend (gameErrorText e) errs

data UserCommandResult
  = Quit
  | SaveGame FilePath
  | LoadGame FilePath
  | Update
  deriving (Eq, Show)

handleUserCommand :: GameState -> UserCommandResult
handleUserCommand state' =
  let inputTxt = state' ^. inputBuffer
      input = T.split isSpace inputTxt
  in case input of
    ("$quit":_) -> Quit
    ("$save":fname:_) -> SaveGame . T.unpack $ fname
    ("$load":fname:_) -> LoadGame . T.unpack $ fname
    _       -> Update

class Monad m => MonadReadFile m where
  readAFile :: FilePath -> m Text

instance MonadReadFile IO where
  readAFile = T.readFile

type StubMonadReadFile = [(FilePath, Text)]

instance Monad m => MonadReadFile (StateT StubMonadReadFile m) where
  readAFile fpath = do
    stubFiles <- get
    case lookup fpath stubFiles of
      Nothing  -> error "No such file"
      Just txt -> pure txt

class Monad m => MonadWriteFile m where
  writeAFile :: FilePath -> Text -> m ()

instance MonadWriteFile IO where
  writeAFile = T.writeFile

instance MonadIO m => MonadWriteFile (WriterT [(FilePath, Text)] m) where
  writeAFile fpath txt = tell [(fpath, txt)]

-- Special Commands

saveGameState :: MonadWriteFile m => FilePath -> GameState -> ExceptT GameError m (FilePath, Text)
saveGameState fpath gameState = do
  let saveFileContents = T.decodeUtf8 . LBS.toStrict . JSON.encode $ gameState
  lift . writeAFile fpath $ saveFileContents
  pure (fpath, saveFileContents)

loadGameState :: MonadReadFile m => FilePath -> ExceptT GameError m GameState
loadGameState fileName = do
  rawContents <- lift . readAFile $ "~/.adventure-engine" </> "saves" </> fileName
  case JSON.eitherDecode' . LBS.fromStrict . T.encodeUtf8 $ rawContents of
    Left decodeError -> throwError . SaveLoadError . T.pack $ decodeError
    Right gameState -> pure gameState

ensureDirectories :: IO ()
ensureDirectories = do
  userHomeRoot <- getHomeDirectory
  createDirectoryIfMissing False $ userHomeRoot </> ".adventure-engine"
  createDirectoryIfMissing False $ userHomeRoot </> ".adventure-engine" </> "saves"

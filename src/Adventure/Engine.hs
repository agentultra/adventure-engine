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

module Adventure.Engine
  ( module Adventure.Engine
  , module Adventure.Engine.Objects
  )
where

import Control.Exception
import Control.Error (note)
import Control.Lens hiding ((.=))
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Aeson ((.=), (.:), decodeFileStrict')
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
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

import Adventure.Data.Map.Util
import Adventure.Engine.Database
import Adventure.Engine.Language
import Adventure.Engine.Objects
import Adventure.Engine.Rewards
import Adventure.List.Utils

itemObjectVerbAliasMap :: GameObject -> Maybe (Map Verb Verb)
itemObjectVerbAliasMap (GameObject _ _ (ObjectItem item'))
  = pure . M.fromList . _itemVerbs $ item'
itemObjectVerbAliasMap _ = Nothing

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
  , _worldPlayerRoom  :: EntityId Room
  , _playerInventory  :: Map Text (EntityId GameObject)
  , _playerVerbs      :: VerbAliasMap
  , _worldPlayerMessages :: [Text]
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
currentRoom w = fetch SpaceWizard (_worldPlayerRoom w) (_worldRooms w)

exitCurrentRoom
  :: ( MonadState GameState m, Monad m )
  => Text -> ExceptT GameError m (EntityId Exit, Exit)
exitCurrentRoom exitName = do
  (GameState w _ _ _ _ _ _ _) <- lift get
  room <- currentRoom w
  exitId <- fetch (ExitDoesNotExist' exitName) exitName (_roomExits room)
  exit <- fetch SpaceWizard exitId (_worldExits w)
  pure (exitId, exit)

getObjectInCurrentRoom
  :: Monad m
  => World
  -> Text
  -> ExceptT GameError m (EntityId GameObject)
getObjectInCurrentRoom w objectName = do
  room <- currentRoom w
  fetch (ObjectNotInRoom objectName) objectName $ _roomObjects room

updateCurrentRoom
  :: ( Monad m, MonadState GameState m )
  => (Room -> Room)
  -> ExceptT GameError m ()
updateCurrentRoom update = do
  g@(GameState w _ _ _ _ _ _ _) <- lift get
  let roomMap = _worldRooms w
  lift $ put g
    { _gameStateWorld = w
      { _worldRooms = M.update (Just . update) (_worldPlayerRoom w ) roomMap
      }
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

getObjectByName
  :: ( MonadState GameState m, Monad m)
  => Text
  -> ExceptT GameError m (EntityId GameObject, GameObject)
getObjectByName objectName = do
  (GameState world _ _ _ _ _ _ _) <- lift get
  objectId <- maybeThrow (ObjectNotInInventory objectName) $
    M.lookup objectName (_playerInventory world)
  object <- maybeThrow (ObjectNotInInventory objectName) $
    M.lookup objectId (_worldObjects world)
  pure (objectId, object)

putObjectInInventory
  :: Text
  -> EntityId GameObject
  -> Map Text (EntityId GameObject)
  -> Map Text (EntityId GameObject)
putObjectInInventory objectName = M.insert (T.toLower objectName)

getObject :: Monad m => World -> EntityId GameObject -> ExceptT GameError m GameObject
getObject w objectId =
  fetch (ObjectDoesNotExist objectId) objectId $ _worldObjects w

modifyObject
  :: Monad m
  => World
  -> EntityId GameObject
  -> (GameObject -> GameObject)
  -> ExceptT GameError m World
modifyObject w@(World _ objects _ _ _ _ _) objectId f =
  pure $ w { _worldObjects = M.adjust f objectId objects }

getExitId :: Monad m => World -> Text -> ExceptT GameError m (EntityId Exit)
getExitId w exitName =
  let exits = _worldExits w
  in liftEither $ note SpaceWizard $ findInMap (\e -> _exitName e == exitName) exits

modifyExit
  :: Monad m
  => World
  -> EntityId Exit
  -> (Exit -> Exit)
  -> ExceptT GameError m World
modifyExit w@(World _ _ exits _ _ _ _) exitId f =
  pure $ w { _worldExits = M.adjust f exitId exits }

-- TODO (james): Might replace this with accessors into some kind of player state record
getPlayerInventory :: GameState -> [GameObject]
getPlayerInventory gameState =
  case runExcept $ getInventoryObjects $ _gameStateWorld gameState of
    Left err -> error $ "Error getting inventory: " ++ show err
    Right objects -> objects

putInInventory
  :: ( Monad m, MonadState GameState m )
  => Text
  -> EntityId GameObject
  -> ExceptT GameError m ()
putInInventory objectName objectId = do
  g@(GameState world _ _ _ _ _ _ _) <- lift get
  let playerInv = _playerInventory world
  lift . put $ g
    { _gameStateWorld = world
      { _playerInventory = putObjectInInventory objectName objectId playerInv
      }
    }

addPlayerMessage
  :: ( MonadState GameState m, Monad m )
  => Text
  -> ExceptT GameError m ()
addPlayerMessage msg = do
  g@(GameState w _ _ _ _ _ _ _) <- lift get
  lift . put $ g { _gameStateWorld = w { _worldPlayerMessages = msg : _worldPlayerMessages w } }

setGameEnd
  :: ( Monad m, MonadState GameState m )
  => GameEndReward
  -> ExceptT GameError m ()
setGameEnd gameEndReward = do
  g <- lift get
  lift . put $ g { _gameStateIsGameEnd = Just gameEndReward }

-- TODO (james): a better way to show errors
data GameState
  = GameState
  { _gameStateWorld          :: World
  , _gameStateScenes         :: [Scene]
  , _gameStateInputBuffer    :: Text
  , _gameStateGameErrors     :: [Text]
  , _gameStateEventLog       :: [Event]
  , _gameStateRewards        :: [EventReward]
  , _gameStateGameEndRewards :: NonEmpty GameEndReward
  , _gameStateIsGameEnd      :: Maybe GameEndReward
  }
  deriving (Eq, Show)

instance JSON.ToJSON GameState where
  toJSON (GameState world scenes _ _ eventLog rewards gameEndRewards isGameEnd) =
    JSON.object
    [ "world" .= world
    , "scenes" .= scenes
    , "events" .= eventLog
    , "rewards" .= rewards
    , "gameEndRewards" .= gameEndRewards
    , "gameEnd" .= isGameEnd
    ]

instance JSON.FromJSON GameState where
  parseJSON = JSON.withObject "GameState" $ \v -> do
    gameEndRewards <- v .: "gameEndRewards"
    if null gameEndRewards
      then fail "Empty game end rewards detected when loading game state"
      else GameState
           <$> v .: "world"
           <*> v .: "scenes"
           <*> pure mempty
           <*> pure mempty
           <*> v .: "events"
           <*> v .: "rewards"
           <*> (pure . NE.fromList $ gameEndRewards)
           <*> v .: "gameEnd"

emitEvent :: (Monad m, MonadState GameState m) => Event -> ExceptT GameError m ()
emitEvent event =
  lift $ modify (\g@(GameState _ _ _ _ eventLog _ _ _) -> g { _gameStateEventLog = eventLog ++ [event]})

newtype GameEngine m a = GameEngine { runEngine :: ExceptT GameError (StateT GameState m) a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadState GameState
    , MonadError GameError
    )

-- Managing the World

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
  , (Verb "use", Verb "use")
  , (Verb "u", Verb "use")
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

defaultGameState :: World -> GameState
defaultGameState world =
  GameState
  world
  []
  ""
  []
  []
  []
  (NE.fromList [GameEndReward (Dug (EntityId 0) (Just $ EntityId 9)) "YOU WON" "WOOHOO"])
  Nothing

initialGameState :: World -> [EventReward] -> Either GameError GameState
initialGameState world eventRewards = do
  initialScene <- runExcept $ render world
  pure
    $ (defaultGameState world)
    { _gameStateScenes = [initialScene]
    , _gameStateRewards = eventRewards
    }

checkGameEnd
  :: ( Monad m, MonadState GameState m )
  => ExceptT GameError m ()
checkGameEnd = do
  g <- lift get
  case gameEnd (last . _gameStateEventLog $ g) (_gameStateGameEndRewards g) of
    Nothing -> pure ()
    Just gameOverText -> setGameEnd gameOverText

handle' :: (Monad m, MonadState GameState m) => ExceptT GameError m ()
handle' = do
  (GameState world _ input _ _ _ _ _) <- lift get
  (v, args) <- parse input
  verb <- getVerb (_playerVerbs world) v
  handleVerb verb args
  checkGameEnd

data GameError
  = RoomDoesNotExist (EntityId Room)
  | ObjectDoesNotExist (EntityId GameObject)
  | ObjectNotInRoom Text
  | ObjectNotInInventory Text
  | ObjectNotInContainer Text
  | InvalidObjectParameter Text
  | InvalidObjectInteraction Text Text Command
  | ExitDoesNotExist (EntityId Exit)
  | ExitDoesNotExist' Text
  | MissingCommand
  | InvalidCommand Text
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
  InvalidObjectInteraction s p _ -> "I don't know how to use " <> s <> " on " <> p
  InvalidCommand _           -> "I don't think I understand!"
  ExitDoesNotExist _         -> "I don't see that exit here."
  ExitDoesNotExist' txt      -> "I don't see an exit called, '" <> txt <> "' here."
  MissingCommand             -> "Did you mean to give me a command?"
  UnrecognizedVerb v         -> "I don't know what you mean by, '" <> unVerb v <> "'."
  MissingParameter txt       -> "I'm missing more information about, '" <> txt <> "'."
  SaveLoadError txt          -> "Error loading save file: " <> txt
  SaveFileError txt          -> "Error saving file: " <> txt
  SpaceWizard                -> "SPACE WIZARD!!!!"

handleVerb :: (Monad m, MonadState GameState m) => Verb -> [Text] -> ExceptT GameError m ()
handleVerb (Verb "walk") args = handleWalk args
handleVerb (Verb "pickup") args = handlePickup args
handleVerb (Verb "drop") args = handleDrop args
handleVerb (Verb "look") args = handleLook args
handleVerb (Verb "examine") args = handleExamine args
handleVerb (Verb "take") args = handleTake args
handleVerb (Verb "dig") args = handleDig args
handleVerb (Verb "use") args = handleUse args
handleVerb v _ = throwError $ UnrecognizedVerb v

handleWalk
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleWalk [] = throwError $ MissingParameter "missing destination"
handleWalk args = do
  (GameState world _ _ _ _ _ _ _) <- lift get
  let rooms      = _worldRooms world
      playerRoom = _worldPlayerRoom world
      exitName   = keyArg args

  (exitId, exit) <- exitCurrentRoom exitName
  _ <- maybeThrow (RoomDoesNotExist (_exitFrom exit)) $
    M.lookup (_exitFrom exit) rooms
  _ <- maybeThrow (RoomDoesNotExist (_exitTo exit)) $
    M.lookup (_exitTo exit) rooms
  case _exitLock exit of
    Nothing -> movePlayer exit playerRoom
    Just (Lock keyItems errText successText Locked)
      | playerHasKeyItems (M.elems . _playerInventory $ world) keyItems -> do
        addPlayerMessage successText
        unlockDoor exitId
        emitEvent $ DoorUnlocked exitId
        movePlayer exit playerRoom
      | otherwise -> addPlayerMessage errText
    Just (Lock _ _ _ Unlocked) -> movePlayer exit playerRoom
  where
    movePlayer :: (MonadState GameState m, Monad m)
      => Exit
      -> EntityId Room
      -> ExceptT GameError m ()
    movePlayer ext rm
      | _exitFrom ext == rm = do
          g@(GameState w _ _ _ _ _ _ _) <- lift get
          let w' = w { _worldPlayerRoom = _exitTo ext }
          lift . put $ g { _gameStateWorld = w' }
          emitEvent $ PlayerMoved (_exitFrom ext) (_exitTo ext)
      | otherwise           = throwError SpaceWizard
    unlockDoor
      :: (MonadState GameState m, Monad m)
      => EntityId Exit
      -> ExceptT GameError m ()
    unlockDoor eid = do
      gameState <- lift get
      w' <- modifyExit (_gameStateWorld gameState) eid unlockExit
      lift . put $ gameState { _gameStateWorld = w' }
    unlockExit :: Exit -> Exit
    unlockExit exit@(Exit _ _ _ _ (Just lck)) =
      exit { _exitLock = Just lck { _lockState = Unlocked } }
    unlockExit exit = exit
    playerHasKeyItems :: [EntityId GameObject] -> [EntityId GameObject] -> Bool
    playerHasKeyItems playerInventory exitKeyItems =
      let exitEntityIds = map getEntityId exitKeyItems
      in exitEntityIds `intersect` map getEntityId playerInventory == exitEntityIds

handlePickup
  :: ( Monad m, MonadState GameState m )
  => [Text]
  -> ExceptT GameError m ()
handlePickup [] = throwError $ MissingParameter "pickup what?"
handlePickup args = do
  g@(GameState world _ _ _ _ _ _ _) <- lift get
  let objectName = keyArg args
      playerRoom = _worldPlayerRoom world
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
  let w' = world
           { _worldRooms = M.adjust (removeItem objectName) playerRoom rooms
           , _playerInventory = putObjectInInventory objectName objectId playerInv
           , _playerVerbs = M.union (_playerVerbs world) itemVerbs
           }
  lift . put $ g { _gameStateWorld = w' }
  emitEvent (ItemPickedUp objectId playerRoom)
  where
    removeItem objectName r = r
      { _roomObjects = M.update (const Nothing) objectName (_roomObjects r)
      }

handleLook
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleLook [] = pure ()
handleLook args =
  case peek "in" args of
    Just args' -> lookInContainer . keyArg $ args'
    Nothing -> do
      let exitName = keyArg args
      (exitId, exit) <- exitCurrentRoom exitName
      addPlayerMessage . _exitDescription $ exit
      emitEvent $ PlayerLookedAtExit exitId

lookInContainer
  :: ( MonadState GameState m, Monad m )
  => Text -> ExceptT GameError m ()
lookInContainer containerName = do
  (GameState w _ _ _ _ _ _ _) <- lift get
  containerId <- getObjectInCurrentRoom w containerName
  container'  <- getObject w containerId
  case _gameObjectObject container' of
    ObjectItem _ -> throwError $ InvalidObjectParameter containerName
    ObjectContainer cont ->
      case _containerLock cont of
        Nothing -> doLookInContainer containerId cont
        Just (ContainerLock lck _) ->
          case _lockState lck of
            Locked ->
              let lockMsg = _lockFailMsg lck
              in addPlayerMessage lockMsg
            Unlocked -> doLookInContainer containerId cont
  where
    doLookInContainer
      :: ( MonadState GameState m, Monad m)
      => EntityId GameObject
      -> Container
      -> ExceptT GameError m ()
    doLookInContainer contId cont = do
      (GameState w _ _ _ _ _ _ _) <- lift get
      items <- traverse (getObject w) $ M.elems (_containerItems cont)
      let itemNames = T.intercalate ", " . map _gameObjectName $ items
      addPlayerMessage $ "Inside the " <> containerName <> " you see: " <> itemNames
      emitEvent $ PlayerLookedInContainer contId

handleDrop
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleDrop [] = throwError $ MissingParameter "drop what?"
handleDrop args = do
  g@(GameState world _ _ _ _ _ _ _) <- lift get
  let playerPos   = _worldPlayerRoom world
      playerInv   = _playerInventory world
      playerVerbs = _playerVerbs world
      rooms       = _worldRooms world
      objectName  = keyArg args

  (objectId, object) <- getObjectByName objectName

  let objectVerbs = fromMaybe M.empty . itemObjectVerbAliasMap $ object

  let w' = world
        { _worldRooms = M.adjust (addObject objectName objectId) playerPos rooms
        , _playerInventory = M.delete objectName playerInv
        , _playerVerbs = M.difference playerVerbs objectVerbs
        }
  lift . put $ g { _gameStateWorld = w' }
  emitEvent $ ItemDropped objectId playerPos
  where
    addObject objectName objectId r
      = r { _roomObjects = M.insert objectName objectId (_roomObjects r) }

handleExamine
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleExamine [] = throwError $ MissingParameter "examine what?"
handleExamine args =
  let objectName = maybe (keyArg args) keyArg $ peek "my" args
  in case peek "my" args of
    Just _ ->
      examineInInventory objectName
    Nothing    ->
      examineInRoom objectName

examineInInventory
  :: ( MonadState GameState m, Monad m)
  => ItemName
  -> ExceptT GameError m ()
examineInInventory objectName = do
  (GameState world _ _ _ _ _ _ _) <- lift get
  objectId <- getObjectInInventory world objectName
  object <- getObject world objectId

  addPlayerMessage . _gameObjectDescription $ object
  emitEvent $ ItemExamined objectId

examineInRoom
  :: ( MonadState GameState m, Monad m )
  => ItemName
  -> ExceptT GameError m ()
examineInRoom objectName = do
  (GameState world _ _ _ _ _ _ _) <- lift get
  objectId <- getObjectInCurrentRoom world objectName
  object   <- getObject world objectId

  addPlayerMessage . describeGameObject $ object
  emitEvent $ ItemExamined objectId

handleTake
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleTake args = do
  g@(GameState w _ _ _ _ _ _ _) <- lift get
  case splitAtWord "from" args of
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

          let w' = w
                { _worldObjects = M.insert objectId object' objects
                , _playerInventory = putObjectInInventory itemName itemId playerInv
                }
          lift . put $ g { _gameStateWorld = w' }
          emitEvent $ ItemTakenFromContainer itemId objectId

handleDig
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleDig _ = do
  (GameState world _ _ _ _ _ _ _) <- lift get
  let roomId = _worldPlayerRoom world
  room <- currentRoom world
  case _roomDig room of
    Left failMsg -> addPlayerMessage failMsg
    Right [] -> do
      addPlayerMessage "You dig but nothing seems to come of it..."
      emitEvent $ Dug roomId Nothing
    Right ((successMsg, mItem):rest) -> do
      updateCurrentRoom $ \r -> r { _roomDig = rest <$ _roomDig r }
      case mItem of
        Nothing -> do
          addPlayerMessage successMsg
          emitEvent $ Dug roomId Nothing
        Just itemId -> do
          object <- getObject world itemId
          putInInventory (_gameObjectName object) itemId
          addPlayerMessage successMsg
          emitEvent $ Dug roomId (Just itemId)

data Command = Unlock deriving (Eq, Show)

data ObjectInteraction
  = ObjectInteraction
  { objectInteractionSubject   :: EntityId GameObject
  , objectInteractionPredicate :: EntityId GameObject
  , objectInteractionCommand   :: Verb
  }
  deriving (Eq, Show)

evalObjectInteraction
  :: ( MonadState GameState m, Monad m )
  => ObjectInteraction
  -> ExceptT GameError m ()
evalObjectInteraction (ObjectInteraction subjectId predicateId command) = do
  (GameState world _ _ _ _ _ _ _) <- lift get
  subjectObject <- getObject world subjectId
  predicateObject <- getObject world predicateId
  doObjectCommand subjectObject predicateObject command
  where
    doObjectCommand
      :: ( MonadState GameState m, Monad m )
      => GameObject
      -> GameObject
      -> Verb
      -> ExceptT GameError m ()
    doObjectCommand subjObj predObj (Verb "use") = do
      g@(GameState world _ _ _ _ _ _ _) <- lift get
      case _gameObjectObject predObj of
        ObjectItem _ ->
          throwError
          $ InvalidObjectInteraction
          (_gameObjectName subjObj)
          (_gameObjectName predObj)
          Unlock
        ObjectContainer c ->
          case _containerLock c of
            Nothing -> throwError $ InvalidCommand (_gameObjectName predObj <> " is not locked!")
            Just (ContainerLock lck _) | subjectId `elem` _lockKeyItems lck -> do
                       let world' = world { _worldObjects = M.adjust (unlock lck) predicateId (_worldObjects world)
                                          , _worldPlayerMessages = _lockSuccessMsg lck : _worldPlayerMessages world
                                          }
                       lift . put $ g { _gameStateWorld = world' }
                       emitEvent $ ContainerUnlocked predicateId subjectId
            Just (ContainerLock lck _) -> do
              let unlockError = _lockFailMsg lck
              addPlayerMessage unlockError
              emitEvent $ ContainerUnlockFailed predicateId subjectId
    doObjectCommand _ _ _ = throwError $ InvalidCommand ""

    unlock :: Lock -> GameObject -> GameObject
    unlock l@(Lock _ _ _ s) o@(GameObject _ _ (ObjectContainer c)) =
      let newLockState = l { _lockState = flipLockState s }
      in o { _gameObjectObject = ObjectContainer c { _containerLock = (\cl -> cl { _containerLockLock = newLockState }) <$> _containerLock c } }
    unlock _ o = o

    flipLockState :: LockState -> LockState
    flipLockState Locked = Unlocked
    flipLockState Unlocked = Locked

handleUse
  :: ( MonadState GameState m, Monad m )
  => [Text]
  -> ExceptT GameError m ()
handleUse args = do
  (GameState world _ _ _ _ _ _ _) <- lift get
  case parseArgs args of
    Just (objectName, predicateName) -> do
      objectId <- getObjectInInventory world objectName
      predId <- getObjectInCurrentRoom world predicateName
      evalObjectInteraction $ ObjectInteraction objectId predId (Verb "use")
    Nothing -> addPlayerMessage "I don't know how to do that..."
  where
    parseArgs :: [Text] -> Maybe (Text, Text)
    parseArgs [] = Nothing
    parseArgs args' =
      case break (== "on") args' of
        ([], _) -> Nothing
        (_, []) -> Nothing
        result  -> do
          let (objectName, predicateName) = tupMap T.unwords (T.unwords . drop 1) result
          if T.null objectName || T.null predicateName
            then Nothing
            else Just (objectName, predicateName)

    tupMap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
    tupMap f g (x, y) = (f x, g y)

render :: Monad m => World -> ExceptT GameError m Scene
render w@(World _ _ exits _ _ _ messages) = do
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
updateGame gameState =
  case runIdentity . (`runStateT` gameState) . runExceptT $ handle' of
    (Left err, g) -> updateGameErrors g err
    (Right _, g@(GameState world' rvs _ _ _ _ _ _)) ->
      case runExcept $ render world' of
        Left renderErr -> updateGameErrors g renderErr
        Right rendered ->
          g & scenes .~ rendered : rvs
            & world .~ world' { _worldPlayerMessages = [] }
            & inputBuffer .~ ""
  where
    updateGameErrors :: GameState -> GameError -> GameState
    updateGameErrors g@(GameState _ _ _ errs _ _ _ _) e
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

loadWorld :: IO World
loadWorld = do
  w <- decodeFileStrict' $ "data" </> "world.json"
  case w of
    Nothing ->
      -- TODO (james): add better error experience for user
      error "Cannot read world.json"
    Just world' -> pure world'

-- | Load the 'EventReward' defined by the author for initializing the
-- game engine state.
loadEventRewards :: (MonadIO m, Monad m) => m [EventReward]
loadEventRewards = do
  maybeRewards <- liftIO . decodeFileStrict' $ "data" </> "event_rewards.json"
  case maybeRewards of
    Nothing ->
      -- TODO (james): add better error experience for user
      error "Cannot read event_rewards.json"
    Just eventRewards -> pure eventRewards

ensureDirectories :: IO ()
ensureDirectories = do
  userHomeRoot <- getHomeDirectory
  createDirectoryIfMissing False $ userHomeRoot </> ".adventure-engine"
  createDirectoryIfMissing False $ userHomeRoot </> ".adventure-engine" </> "saves"

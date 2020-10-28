{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Adventure.Engine where

import Data.Bifunctor
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
  { _containerSize :: Int
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

data World
  = World
  { _worldRooms      :: Map (EntityId Room) Room
  , _worldObjects    :: Map (EntityId GameObject) GameObject
  , _worldExits      :: Map (EntityId Exit) Exit
  , _playerRoom      :: EntityId Room
  , _playerInventory :: Map Text (EntityId GameObject)
  , _logMessages     :: [Text]
  }
  deriving (Eq, Show)

currentRoom :: World -> Either GameError Room
currentRoom w = maybeToRight SpaceWizard
  $ M.lookup (_playerRoom w) (_worldRooms w)

exitCurrentRoom :: Text -> World -> Either GameError Exit
exitCurrentRoom exitName w = do
  room <- currentRoom w
  exitId <- maybeToRight (ExitDoesNotExist' exitName)
    $ M.lookup exitName (_roomExits room)
  maybeToRight SpaceWizard $ M.lookup exitId (_worldExits w)

newtype GameState
  = GameState
  { _gameStateCommands :: [Command]
  }

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

renderRoom :: Room -> [GameObject] -> [Exit] -> [GameObject] -> [Text] -> Text
renderRoom (Room name desc _ _) objects exits invItems msgs = T.unlines
  [ name
  , "-----------"
  , desc
  , "----------"
  , T.unlines msgs
  , "----------"
  , "You see: " <> T.intercalate ", " (_gameObjectName <$> objects)
  , "You are holding: " <> T.intercalate ", " (_gameObjectName <$> invItems)
  , "Possible exits: " <> T.intercalate ", " (_exitName <$> exits)
  ]

-- Managing the World

type ItemName = Text

data Command
  = Command
  { _commandVerb :: Verb
  , _commandWith :: World -> [Text] -> Either GameError World
  }

getCommand :: [Command] -> Verb -> Either GameError Command
getCommand legalCommands v@(Verb v') =
  maybeToRight (UnrecognizedCommand v) $ find (\(Command (Verb cv) _) -> T.isPrefixOf v' cv) legalCommands

parse :: Text -> Either GameError (Verb, [Text])
parse = parseCmd . T.words
  where
    parseCmd [] = Left MissingCommand
    parseCmd (x:xs) = pure (Verb x, xs)

defaultGameState :: GameState
defaultGameState
  = GameState
  [ walkTo
  , pickUp
  , dropTo
  , look
  , examine
  , takeFrom
  ]

handle' :: GameState -> World -> Text -> Either GameError World
handle' game world input = do
  (v, args) <- first (const MissingCommand) . parse $ input
  cmd <- first (const $ UnrecognizedCommand v)
    $ getCommand (_gameStateCommands game) v
  _commandWith cmd world args

data GameError
  = RoomDoesNotExist (EntityId Room)
  | ObjectDoesNotExist Text
  | ObjectNotInRoom Text
  | ObjectNotInInventory Text
  | ObjectNotInContainer Text
  | InvalidObjectParameter Text
  | ExitDoesNotExist (EntityId Exit)
  | ExitDoesNotExist' Text
  | MissingCommand
  | UnrecognizedCommand Verb
  | MissingParameter Text
  | SpaceWizard
  deriving (Eq, Show)

type CommandHandler = World -> [Text] -> Either GameError World

walkTo :: Command
walkTo = Command (Verb "walk") handleWalk
  where
    handleWalk :: CommandHandler
    handleWalk _ [] = Left $ MissingParameter "missing destination"
    handleWalk world args = do
      let rooms      = _worldRooms world
          playerRoom = _playerRoom world
          exitName   = keyArg args

      exit <- exitCurrentRoom exitName world
      _ <- maybeToRight (RoomDoesNotExist (_exitFrom exit)) $
        M.lookup (_exitFrom exit) rooms
      _ <- maybeToRight (RoomDoesNotExist (_exitTo exit)) $
        M.lookup (_exitTo exit) rooms
      if _exitFrom exit /= playerRoom
        then Left SpaceWizard
        else pure $ world { _playerRoom = _exitTo exit }

pickUp :: Command
pickUp = Command (Verb "pickup") handlePickup
  where
    handlePickup :: CommandHandler
    handlePickup _ [] = Left $ MissingParameter "pickup what?"
    handlePickup world args = do
      let objectName = keyArg args
          playerRoom = _playerRoom world
          playerInv = _playerInventory world
          rooms = _worldRooms world
          objects = _worldObjects world

      room <- currentRoom world
      objectId <- maybeToRight (ObjectNotInRoom objectName) $
        M.lookup objectName (_roomObjects room)
      _ <- maybeToRight SpaceWizard $
        M.lookup playerRoom rooms
      _ <- maybeToRight (ObjectDoesNotExist objectName) $
        M.lookup objectId objects
      pure $ world
        { _worldRooms = M.adjust (removeItem objectName) playerRoom rooms
        , _playerInventory = M.insert objectName objectId playerInv
        }
    removeItem objectName r = r
      { _roomObjects = M.update (const Nothing) objectName (_roomObjects r)
      }

look :: Command
look = Command (Verb "look") handleLook
  where
    handleLook :: CommandHandler
    handleLook w [] = pure w
    handleLook w args =
      let exits = _worldExits w
      in case peek "in" args of
        Just args' -> lookInContainer w . keyArg $ args'
        Nothing -> do
          let exitName = keyArg args
          room <- currentRoom w
          exitId <- maybeToRight SpaceWizard $
            M.lookup exitName (_roomExits room)
          exit <- maybeToRight SpaceWizard $
            M.lookup exitId exits
          pure $ w { _logMessages = _logMessages w <> [_exitDescription exit] }

lookInContainer :: World -> Text -> Either GameError World
lookInContainer w containerName = do
  room <- currentRoom w
  containerId <- maybeToRight (ObjectNotInRoom containerName)
    $ M.lookup containerName (_roomObjects room)
  container' <- maybeToRight (ObjectDoesNotExist containerName)
    $ M.lookup containerId (_worldObjects w)
  case _gameObjectObject container' of
    ObjectItem _ -> Left $ InvalidObjectParameter containerName
    ObjectContainer cont -> do
      items <- traverse getObject $ M.elems (_containerItems cont)
      let itemNames = T.intercalate ", " . map _gameObjectName $ items
      pure $ w
        { _logMessages = _logMessages w
          <> ["Inside the " <> containerName <> " you see: " <> itemNames]
        }
      where
        getObject objectId =
          case M.lookup objectId (_worldObjects w) of
            Nothing    -> Left $ ObjectDoesNotExist containerName
            Just item' -> Right item'

dropTo :: Command
dropTo = Command (Verb "drop") handleDrop
  where
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
    addObject objectName objectId r = r { _roomObjects = M.insert objectName objectId (_roomObjects r) }

examine :: Command
examine = Command (Verb "examine") handleExamine
  where
    handleExamine :: CommandHandler
    handleExamine _ [] = Left $ MissingParameter "examine what?"
    handleExamine world args = do
      let playerInv  = _playerInventory world
          objectName = maybe (keyArg args) keyArg $ peek "my" args

      case peek "my" args of
        Just _ -> do
          objectId <- maybeToRight (ObjectNotInInventory objectName) $
            M.lookup objectName playerInv
          examineInInventory world objectName objectId
        Nothing    -> do
          room <- currentRoom world
          objectId <- maybeToRight (ObjectNotInRoom objectName) $
            M.lookup objectName (_roomObjects room)
          examineInRoom world objectName objectId

examineInRoom :: World -> ItemName -> EntityId GameObject -> Either GameError World
examineInRoom world objectName objectId = do
  let objects     = _worldObjects world
      msgs      = _logMessages world

  room <- currentRoom world
  _ <- maybeToRight (ObjectNotInRoom objectName) $
    M.lookup objectName (_roomObjects room)
  object <- maybeToRight (ObjectDoesNotExist objectName) $
    M.lookup objectId objects

  pure $ world { _logMessages = msgs <> [_gameObjectDescription object] }

examineInInventory :: World -> ItemName -> EntityId GameObject -> Either GameError World
examineInInventory world objectName objectId = do
  let objects = _worldObjects world
      msgs  = _logMessages world

  object <- maybeToRight (ObjectNotInInventory objectName) $
    M.lookup objectId objects

  pure $ world { _logMessages = msgs <> [_gameObjectDescription object] }

takeFrom :: Command
takeFrom = Command (Verb "take") handleTake
  where
    handleTake :: CommandHandler
    handleTake w args = case splitAtWord "from" args of
      Nothing       -> Left $ MissingParameter "take what from what?"
      Just (xs, ys) -> do
        let containerName = keyArg ys
            itemName      = keyArg xs
            playerInv     = _playerInventory w
            objects       = _worldObjects w

        room <- currentRoom w
        objectId <- maybeToRight (ObjectNotInRoom containerName) $
          M.lookup containerName (_roomObjects room)
        object <- maybeToRight (ObjectDoesNotExist containerName) $
          M.lookup objectId objects
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

render :: World -> Either GameError Text
render w@(World _ objects exits _ playerInv msgs) = do
  room <- currentRoom w
  objects' <- traverse getItem $ M.elems (_roomObjects room)
  exits' <- traverse getExit $ M.elems . _roomExits $ room
  invObjects <- traverse getItem $ M.elems playerInv
  pure $ renderRoom room objects' exits' invObjects msgs
  where
    getItem objectId  =
      case M.lookup objectId objects of
       Nothing ->
         Left $ ObjectDoesNotExist
         ("Error missing rendering object: " <> (T.pack . show $ objectId))
       Just item' -> Right item'
    getExit exitId =
      case M.lookup exitId exits of
        Nothing   -> Left $ ExitDoesNotExist exitId
        Just exit -> Right exit

repl :: GameState -> World -> IO ()
repl g w = do
  let initialRender = render w
  case initialRender of
    Left err -> do
      print err
      pure ()
    Right rendered -> T.putStrLn rendered
  runInputT defaultSettings $ loop w
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
          case handle' g world . T.pack $ rawInput of
            Left err -> do
              outputStrLn $ show err
              loop world
            Right world' ->
              case render world' of
                Left renderErr -> do
                  outputStrLn $ show renderErr
                  loop world'
                Right rendered -> do
                  outputStrLn . T.unpack $ rendered
                  loop world'

displayWorld :: World -> InputT IO ()
displayWorld world = case render world of
  Left err       -> outputStrLn $ show err
  Right rendered -> outputStrLn . T.unpack $ rendered

newtype Verb = Verb { unVerb :: Text }
  deriving (Eq, Show)

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

eitherToInput :: (Exception e, MonadException m) => Either e a -> InputT m a
eitherToInput (Left err)     = throwIO err
eitherToInput (Right result) = pure result

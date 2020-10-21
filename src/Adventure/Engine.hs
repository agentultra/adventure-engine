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
  , _roomItems       :: Map Text (EntityId Item)
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
  , _playerInventory :: Map Text (EntityId Item)
  , _logMessages     :: [Text]
  }
  deriving (Eq, Show)

newtype GameState
  = GameState
  { _gameStateCommands :: [Command']
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
  M.empty
  $ M.fromList [("door", EntityId 5)]

shovel :: Item
shovel = Item "Shovel" "A rusted shovel with a wooden handle." 2 4

purse :: Item
purse = Item "Purse" "Weathered, old, leather purse." 1 1

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
  (M.fromList [(EntityId 1, shovel), (EntityId 2, purse)])
  (M.fromList [(EntityId 3, frontDoorOutside), (EntityId 5, frontDoorInside)])
  (EntityId 0)
  M.empty
  mempty

renderRoom :: Room -> [Item] -> [Exit] -> [Item] -> [Text] -> Text
renderRoom (Room name desc _ _) items exits invItems msgs = T.unlines
  [ name
  , "-----------"
  , desc
  , "----------"
  , T.unlines msgs
  , "----------"
  , "You see: " <> T.intercalate ", " (_itemName <$> items)
  , "You are holding: " <> T.intercalate ", " (_itemName <$> invItems)
  , "Possible exits: " <> T.intercalate ", " (_exitName <$> exits)
  ]

-- Managing the World

type ItemName = Text

data Command
  = Walk (EntityId Exit)
  | PickUp ItemName (EntityId Item)
  | Look
  | Drop ItemName (EntityId Item)
  | ExamineInventory ItemName (EntityId Item)
  | ExamineRoom ItemName (EntityId Item)
  deriving (Eq, Show)

data Command'
  = Command'
  { _commandVerb :: Verb
  , _commandWith :: World -> [Text] -> Either GameError World
  }

data Invocation
  = Invocation
  { _invokeCommand   :: Command'
  , _invokeArguments :: [Text]
  , _invokeResult    :: Either GameError World
  }

getCommand :: [Command'] -> Verb -> Either InputError Command'
getCommand legalCommands v@(Verb v') =
  maybeToRight (InvalidVerb' v) $ find (\(Command' (Verb cv) _) -> T.isPrefixOf v' cv) legalCommands

parse :: Text -> Either InputError (Verb, [Text])
parse = parseCmd . T.words
  where
    parseCmd [] = Left EmptyInput
    parseCmd (x:xs) = pure (Verb x, xs)

defaultGameState :: GameState
defaultGameState
  = GameState
  [ walkTo'
  ]

fooCommand :: Command'
fooCommand = Command' (Verb "foo") handleFoo

handleFoo :: World -> [Text] -> Either GameError World
handleFoo world _ = pure $ world { _logMessages = _logMessages world <> ["Hello from foo!"] }


handle' :: GameState -> World -> Text -> Either GameError World
handle' game world input = do
  (v, args) <- first (const MissingCommand) . parse $ input
  cmd <- first (const $ UnrecognizedCommand v)
    $ getCommand (_gameStateCommands game) v
  _commandWith cmd world args

data GameError
  = RoomDoesNotExist (EntityId Room)
  | ItemDoesNotExist (EntityId Item)
  | ItemNotInRoom (EntityId Item) (EntityId Room)
  | ItemNotInInventory ItemName (EntityId Item)
  | ExitDoesNotExist (EntityId Exit)
  | ExitDoesNotExist' Text
  | MissingCommand
  | UnrecognizedCommand Verb
  | MissingParameter Text
  | SpaceWizard
  deriving (Eq, Show)

type CommandHandler = World -> [Text] -> Either GameError World

walkTo' :: Command'
walkTo' = Command' (Verb "walk") handleWalk
  where
    handleWalk :: CommandHandler
    handleWalk _ [] = Left $ MissingParameter "missing destination"
    handleWalk world args = do
      let exits      = _worldExits world
          rooms      = _worldRooms world
          playerRoom = _playerRoom world
          exitName   = T.unwords . map T.toLower $ args

      room <- maybeToRight SpaceWizard
        $ M.lookup playerRoom rooms
      exitId <- maybeToRight
        (ExitDoesNotExist' exitName)
        $ M.lookup exitName (_roomExits room)
      exit <- maybeToRight SpaceWizard $
        M.lookup exitId exits
      _ <- maybeToRight (RoomDoesNotExist (_exitFrom exit)) $
        M.lookup (_exitFrom exit) rooms
      _ <- maybeToRight (RoomDoesNotExist (_exitTo exit)) $
        M.lookup (_exitTo exit) rooms
      if _exitFrom exit /= playerRoom
        then Left SpaceWizard
        else pure $ world { _playerRoom = _exitTo exit }

pickUp' :: Command'
pickUp' = Command' (Verb "pickup") handlePickup
  where
    handlePickup :: CommandHandler
    handlePickup = undefined

look' :: Command'
look' = Command' (Verb "pickup") handlePickup
  where
    handlePickup :: CommandHandler
    handlePickup = undefined

drop' :: Command'
drop' = Command' (Verb "pickup") handlePickup
  where
    handlePickup :: CommandHandler
    handlePickup = undefined

examine' :: Command'
examine' = Command' (Verb "pickup") handlePickup
  where
    handlePickup :: CommandHandler
    handlePickup = undefined

update :: World -> Command -> Either GameError World
update world = \case
  Walk exitId                           -> walkTo world exitId
  PickUp itemName itemId                -> pickUp world itemName itemId
  Look                                  -> pure world
  Drop itemName itemId                  -> dropTo world itemName itemId
  ExamineInventory itemName itemId ->
    examineInInventory world itemName itemId
  ExamineRoom itemName itemId           ->
    examineInRoom world itemName itemId

walkTo :: World -> EntityId Exit -> Either GameError World
walkTo world@(World rooms _ exits playerRoom _ _) exitId = do
  exit <- maybeToRight (ExitDoesNotExist exitId) $
    M.lookup exitId exits
  _ <- maybeToRight (RoomDoesNotExist (_exitFrom exit)) $
    M.lookup (_exitFrom exit) rooms
  _ <- maybeToRight (RoomDoesNotExist (_exitTo exit)) $
    M.lookup (_exitTo exit) rooms
  if _exitFrom exit /= playerRoom
    then Left SpaceWizard
    else pure $ world { _playerRoom = _exitTo exit }

pickUp :: World -> ItemName -> EntityId Item -> Either GameError World
pickUp world@(World rooms items _ playerRoom playerInv _) itemName itemId = do
  _ <- maybeToRight SpaceWizard $
    M.lookup playerRoom rooms
  -- TODO: add a message, update inventory
  _ <- maybeToRight (ItemDoesNotExist itemId) $
    M.lookup itemId items
  pure $ world
    { _worldRooms = M.adjust removeItem playerRoom rooms
    , _playerInventory = M.insert itemName itemId playerInv
    }
  where
    removeItem r = r
      { _roomItems = M.update (const Nothing) itemName (_roomItems r)
      }

dropTo :: World -> ItemName -> EntityId Item -> Either GameError World
dropTo world itemName itemId = do
  let playerPos = _playerRoom world
      playerInv = _playerInventory world
      rooms     = _worldRooms world

  pure $ world
    { _worldRooms = M.adjust addItem playerPos rooms
    , _playerInventory = M.delete itemName playerInv
    }
  where
    addItem r = r { _roomItems = M.insert itemName itemId (_roomItems r) }

examineInRoom :: World -> ItemName -> EntityId Item -> Either GameError World
examineInRoom world itemName itemId = do
  let playerPos = _playerRoom world
      items     = _worldItems world
      rooms     = _worldRooms world
      msgs      = _logMessages world

  room <- maybeToRight (RoomDoesNotExist playerPos) $
    M.lookup playerPos rooms
  _ <- maybeToRight (ItemNotInRoom itemId playerPos) $
    M.lookup itemName (_roomItems room)
  item <- maybeToRight (ItemDoesNotExist itemId) $
    M.lookup itemId items

  pure $ world { _logMessages = msgs <> [_itemDescription item] }

examineInInventory :: World -> ItemName -> EntityId Item -> Either GameError World
examineInInventory world itemName itemId = do
  let items = _worldItems world
      msgs  = _logMessages world

  item <- maybeToRight (ItemNotInInventory itemName itemId) $
    M.lookup itemId items

  pure $ world { _logMessages = msgs <> [_itemDescription item] }

-- M.update (const Nothing) itemName (_roomItems room)
render :: World -> Either GameError Text
render (World rooms items exits playerRoom playerInv msgs) = do
  room <- maybeToRight (RoomDoesNotExist playerRoom) $
    M.lookup playerRoom rooms
  items' <- traverse getItem $ M.elems (_roomItems room)
  exits' <- traverse getExit $ M.elems . _roomExits $ room
  invItems <- traverse getItem $ M.elems playerInv
  pure $ renderRoom room items' exits' invItems msgs
  where
    getItem itemId  =
      case M.lookup itemId items of
       Nothing   -> Left $ ItemDoesNotExist itemId
       Just item -> Right item
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

parseLine :: Text -> World -> Either InputError Command
parseLine raw world = do
  input <- parseInput raw
  parseCommand input world

displayWorld :: World -> InputT IO ()
displayWorld world = case render world of
  Left err       -> outputStrLn $ show err
  Right rendered -> outputStrLn . T.unpack $ rendered

newtype Verb = Verb { unVerb :: Text }
  deriving (Eq, Show)

verb :: Text -> Either InputError Verb
verb v
  | T.length v > 0 =
    case filter (T.isPrefixOf . T.toLower $ v) verbs of
      []      -> Left InvalidVerb
      [v']    -> Right $ Verb v'
      (_:_:_) -> Left AmbiguousVerb
  | otherwise = Left InvalidVerb

verbs :: [Text]
verbs = ["look", "walk", "pickup", "drop", "examine"]

newtype Preposition = Preposition { unPreposition :: Text }
  deriving (Eq, Show)

preposition :: Text -> Either InputError Preposition
preposition p = case filter (== T.toLower p) prepositions of
  []   -> Left InvalidPreposition
  [p'] -> Right $ Preposition p'
  _    -> Left InvalidPreposition

prepositions :: [Text]
prepositions = ["my"]

data InputError
  = InvalidVerb
  | AmbiguousVerb
  | InvalidPreposition
  | ParseFail
  | UnknownInput
  | UnknownParameter Text
  | EmptyInput
  | InvalidVerb' Verb
  deriving (Eq, Show)

instance Exception InputError

data Input
  = Input
  { _verb        :: Verb
  , _preposition :: Maybe Preposition
  , _parameter   :: Maybe Text
  }
  deriving (Eq, Show)

parseInput :: Text -> Either InputError Input
parseInput = mkInput . T.split (==' ')
  where
    mkInput [] = Left ParseFail
    mkInput [x] = do
      v <- verb x
      pure $ Input v Nothing Nothing
    mkInput (x:y:ys) = do
      v <- verb x
      case preposition y of
        Left _     -> pure $ Input v Nothing (Just (T.toLower . T.unwords $ y : ys))
        Right prep -> pure $ Input v (Just prep) (Just (T.toLower . T.unwords $ ys))

parseCommand :: Input -> World -> Either InputError Command
parseCommand input world =
  case unVerb . _verb $ input of
    "walk" -> do
      room <- maybeToRight UnknownInput
        $ M.lookup (_playerRoom world) (_worldRooms world)
      destName <- maybeToRight UnknownInput $ _parameter input
      exitId <- maybeToRight
        (UnknownParameter . fromJust $ _parameter input)
        $ M.lookup destName (_roomExits room)
      pure $ Walk exitId
    "look" -> pure Look
    "pickup" -> do
      room <- maybeToRight UnknownInput $
        M.lookup (_playerRoom world) (_worldRooms world)
      itemName <- maybeToRight UnknownInput $ _parameter input
      itemId <- maybeToRight UnknownInput $
        M.lookup itemName (_roomItems room)
      pure $ PickUp itemName itemId
    "drop" -> do
      itemName <- maybeToRight UnknownInput $ _parameter input
      itemId <- maybeToRight (UnknownParameter itemName) $
        M.lookup itemName (_playerInventory world)
      pure $ Drop itemName itemId
    "examine" -> do
      itemName <- maybeToRight UnknownInput $ _parameter input
      case _preposition input of
        Nothing -> do
          room <- maybeToRight UnknownInput $
            M.lookup (_playerRoom world) (_worldRooms world)
          itemId <- maybeToRight UnknownInput $
            M.lookup itemName (_roomItems room)
          pure $ ExamineRoom itemName itemId
        Just _  -> do
          itemId <- maybeToRight (UnknownParameter itemName) $
            M.lookup itemName (_playerInventory world)
          pure $ ExamineInventory itemName itemId
    _ -> Left InvalidVerb
{-
walk north
pickup shovel
-}

-- Utilities

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

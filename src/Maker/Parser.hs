{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Maker.Parser where

import Adventure.Engine
import Adventure.Engine.Database
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

newtype MakerParseState = MakerParseState { seenRoomEntityIds :: Set (EntityId Room) }

type Parser = ParsecT Void Text (StateT MakerParseState Identity)

nonEmptyLineParser :: Parser Text
nonEmptyLineParser = do
  str <- someTill latin1Char newline
  pure . T.pack $ str

lineParser :: Parser Text
lineParser = do
  str <- T.pack <$> manyTill latin1Char newline
  pure $ if T.null str then "\n" else str

entityIdParser :: Parser (EntityId a)
entityIdParser = do
  digits <- manyTill digitChar (single '.')
  pure . EntityId $ read digits

roomParser :: Parser (EntityId Room, Room)
roomParser = do
  s@MakerParseState {..} <- lift get
  void . single $ '#'
  space1
  roomId <- entityIdParser @Room
  when (roomId `S.member` seenRoomEntityIds) $ fail "Room IDs must be unique"
  lift . put $ s { seenRoomEntityIds = roomId `S.insert` seenRoomEntityIds }
  roomName <- nonEmptyLineParser
  roomDescLines <- manyTill lineParser $ single '#'
  void newline
  exitIds <- propertyParser1 @Exit "Exits"
  let room = Room
        { _roomName = roomName
        , _roomDescription = T.strip . T.concat $ roomDescLines
        , _roomObjects = mempty
        , _roomExits = M.fromList . map exitRefToMap $ exitIds
        , _roomDig = Left "You can't do that here.."
        , _roomBackground = Nothing
        }
  pure (roomId, room)
  where
    exitRefToMap :: EntityRef Exit -> (Text, EntityId Exit)
    exitRefToMap (EntityRef entityId exitName) = (exitName, entityId)

sectionParser :: Text -> Parser a -> Parser b -> Parser [a]
sectionParser sectionName p sep = do
  void . single $ '['
  void . string' $ sectionName
  void . single $ ']'
  void newline
  sepEndBy p sep

roomSectionParser :: Parser [(EntityId Room, Room)]
roomSectionParser = sectionParser "Rooms" roomParser $ void newline <|> eof

worldParser :: Parser World
worldParser = do
  worldRooms <- roomSectionParser
  pure $ World
    { _worldRooms = M.fromList worldRooms
    , _worldObjects = mempty
    , _worldExits = mempty
    , _worldPlayerRoom = EntityId 0
    , _worldPlayerInventory = mempty
    , _worldPlayerVerbs = mempty
    , _worldPlayerMessages = mempty
    }

-- | Represents a reference to an entity in the game source text.
--
-- Written as @(1. FooName)@ in the source.  We get the type parameter
-- from the context of the parse.  If we're parsing a list of "exits"
-- then we know 'a' must be of type 'Exit'.
data EntityRef a
  = EntityRef
  { _entityRefEntityId :: EntityId a
  , _entityRefName     :: Text
  }
  deriving (Eq, Show)

-- | Parse an entity reference in the source text.
--
-- Callers should determine type parameter of the EntityId.
entityRefParser :: Parser (EntityRef a)
entityRefParser = do
  void . single $ '('
  entityId <- entityIdParser
  space1
  entityName <- T.pack <$> manyTill latin1Char (single ')')
  pure $ EntityRef entityId entityName

propertyParser :: Text -> Parser [EntityRef a]
propertyParser lbl = do
  void . string $ "-- "
  void . string $ lbl
  void . single $ ':'
  space1
  sepEndBy entityRefParser (void . string $ ", ")

propertyParser1 :: Text -> Parser [EntityRef a]
propertyParser1 lbl = do
  void . string $ "-- "
  void . string $ lbl
  void . single $ ':'
  space1
  sepEndBy1 entityRefParser (void . string $ ", ")

runMakerParser :: Parser a -> FilePath -> Text -> Either (ParseErrorBundle Text Void) a
runMakerParser p fpath
  = runIdentity
  . (`evalStateT` MakerParseState S.empty)
  . runParserT p fpath

testMakerParser :: Show a => Parser a -> Text -> IO ()
testMakerParser p txt = do
  let result = runMakerParser p "test" txt
  print result

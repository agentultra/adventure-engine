{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Maker.Parser where

import Adventure.Engine
import Adventure.Engine.Database
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import System.FilePath
import Text.Megaparsec
import Text.Megaparsec.Char

data MakerParseState
  = MakerParseState
  { seenRoomEntityIds :: Set (EntityId Room)
  , seenExitEntityIds :: Set (EntityId Exit)
  }

type Parser = ParsecT Void Text (StateT MakerParseState Identity)

nonEmptyLineParserBy :: Parser a -> Parser Text
nonEmptyLineParserBy sep = do
  str <- someTill latin1Char sep
  pure . T.pack $ str

nonEmptyLineParser :: Parser Text
nonEmptyLineParser = nonEmptyLineParserBy newline

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
  exitIds <- entityRefPropertyListParser1 @Exit "Exits"
  objectIds <- entityRefPropertyListParser @GameObject "Objects"
  bg <- option Nothing backgroundImageParser
  digs <- optional $ propertyListParser "Dig" digParser
  let room = Room
        { _roomName = roomName
        , _roomDescription = T.strip . T.concat $ roomDescLines
        , _roomObjects = M.fromList . map entityRefToMap $ objectIds
        , _roomExits = M.fromList . map entityRefToMap $ exitIds
        , _roomDig = maybe (Left "You can't do that here..") Right digs
        , _roomBackground = bg
        }
  pure (roomId, room)
  where
    entityRefToMap :: EntityRef a -> (Text, EntityId a)
    entityRefToMap (EntityRef entityId entityName) = (entityName, entityId)

backgroundImageParser :: Parser (Maybe Text)
backgroundImageParser = do
  void . string $ "-- Background: "
  bgName <- backgroundFilenameParser
  pure . Just $ bgName

backgroundFilenameParser :: Parser Text
backgroundFilenameParser = do
  path <- nonEmptyLineParser
  if ".bmp" `T.isSuffixOf` path
    then pure . T.pack . takeFileName . T.unpack $ path
    else fail "Unsupported background file extension"

exitParser :: Parser (EntityId Exit, Exit)
exitParser = do
  s@MakerParseState {..} <- lift get
  void . single $ '#'
  space1
  exitId <- entityIdParser @Exit
  when (exitId `S.member` seenExitEntityIds) $ fail "Exit IDs must be unique"
  lift . put $ s { seenExitEntityIds = exitId `S.insert` seenExitEntityIds }
  exitName <- nonEmptyLineParser
  exitDescLines <- manyTill lineParser $ single '#'
  void newline
  exitFromRoomId <- exitRefParser "From" seenRoomEntityIds
  exitToRoomId <- exitRefParser "To" seenRoomEntityIds
  let exit = Exit
        { _exitName = exitName
        , _exitDescription = T.strip . T.concat $ exitDescLines
        , _exitFrom = exitFromRoomId
        , _exitTo = exitToRoomId
        , _exitLock = Nothing
        }
  pure (exitId, exit)
  where
    exitRefParser lbl roomIds = propertyParser lbl $ try $ do
      (EntityRef roomId _) <- entityRefParser @Room
      unless (roomId `S.member` roomIds) $
        fail "Room ID is not defined, check the [Rooms] section"
      pure roomId

sectionParser :: Text -> Parser a -> Parser b -> Parser [a]
sectionParser sectionName p sep = do
  void . single $ '['
  void . string' $ sectionName
  void . single $ ']'
  void newline
  sepEndBy p sep

roomSectionParser :: Parser [(EntityId Room, Room)]
roomSectionParser = sectionParser "Rooms" roomParser $ void newline <|> eof

exitSectionParser :: Parser [(EntityId Exit, Exit)]
exitSectionParser = sectionParser "Exits" exitParser $ void newline <|> eof

worldParser :: Parser World
worldParser = do
  worldRooms <- roomSectionParser
  worldExits <- exitSectionParser
  pure $ World
    { _worldRooms = M.fromList worldRooms
    , _worldObjects = mempty
    , _worldExits = M.fromList worldExits
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

digParser :: Parser DigDefinition
digParser = do
  (digSuccess, maybeDigGameObject) <- try basicDigParser <|> itemDigParser
  pure $ DigDefinition digSuccess (_entityRefEntityId <$> maybeDigGameObject)

basicDigParser :: Parser (Text, Maybe (EntityRef a))
basicDigParser = do
  void . single $ '['
  hspace
  successMsg <- nonEmptyLineParserBy $ single ']'
  case T.find (== '|') successMsg of
    Just _ -> fail "Invalid basic dig success message"
    Nothing -> pure (T.strip successMsg, Nothing)

itemDigParser :: Parser (Text, Maybe (EntityRef a))
itemDigParser = do
  void . single $ '['
  hspace
  successMsg <- nonEmptyLineParserBy $ string "||"
  hspace
  entityRef <- entityRefParser
  hspace
  void . single $ ']'
  pure (T.strip successMsg, Just entityRef)

propertyParser :: Text -> Parser a -> Parser a
propertyParser lbl p = do
  propertyLabelParser lbl
  prop <- p
  void newline
  pure prop

propertyLabelParser :: Text -> Parser ()
propertyLabelParser lbl = do
  void . string $ "-- "
  void . string $ lbl
  void . single $ ':'
  hspace

propertyListParser :: Text -> Parser a -> Parser [a]
propertyListParser lbl p = do
  propertyLabelParser lbl
  ps <- sepEndBy p (void . string $ ", ")
  void newline
  pure ps

entityRefPropertyListParser :: Text -> Parser [EntityRef a]
entityRefPropertyListParser lbl = propertyListParser lbl entityRefParser

propertyListParser1 :: Text -> Parser a -> Parser [a]
propertyListParser1 lbl p = do
  propertyLabelParser lbl
  ps <- sepEndBy1 p (void . string $ ", ")
  void newline
  pure ps

entityRefPropertyListParser1 :: Text -> Parser [EntityRef a]
entityRefPropertyListParser1 lbl = propertyListParser1 lbl entityRefParser

runMakerParser :: Parser a -> FilePath -> Text -> Either (ParseErrorBundle Text Void) a
runMakerParser p fpath
  = runIdentity
  . (`evalStateT` MakerParseState S.empty S.empty)
  . runParserT p fpath

testMakerParser :: Show a => Parser a -> Text -> IO ()
testMakerParser p txt = do
  let result = runMakerParser p "test" txt
  print result

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Maker.Parser where

import Adventure.Engine
import Adventure.Engine.Database
import Control.Monad
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

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
  void . single $ '#'
  space1
  roomId <- entityIdParser @Room
  roomName <- nonEmptyLineParser
  roomDescLines <- manyTill lineParser $ single '#'
  let room = Room
        { _roomName = roomName
        , _roomDescription = T.strip . T.concat $ roomDescLines
        , _roomObjects = mempty
        , _roomExits = mempty
        , _roomDig = Left "You can't do that here.."
        , _roomBackground = Nothing
        }
  pure (roomId, room)

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

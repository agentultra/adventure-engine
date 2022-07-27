{-# LANGUAGE OverloadedStrings #-}

module Maker.Parser where

import Adventure.Engine.Objects
import Control.Monad
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

roomParser :: Parser Room
roomParser = do
  void . single $ '#'
  space1
  roomName <- nonEmptyLineParser
  roomDescLines <- manyTill lineParser $ single '#'
  pure
    $ Room
    { _roomName = roomName
    , _roomDescription = T.strip . T.concat $ roomDescLines
    , _roomObjects = mempty
    , _roomExits = mempty
    , _roomDig = Left "You can't do that here.."
    , _roomBackground = Nothing
    }

sectionParser :: Text -> Parser a -> Parser b -> Parser [a]
sectionParser sectionName p sep = do
  void . single $ '['
  void . string' $ sectionName
  void . single $ ']'
  void newline
  sepEndBy p sep

roomSectionParser :: Parser [Room]
roomSectionParser = sectionParser "Rooms" roomParser $ void newline <|> eof

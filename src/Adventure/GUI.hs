{-# LANGUAGE ScopedTypeVariables #-}
module Adventure.GUI where

import Monomer
import qualified Monomer.Lens as L

import Control.Exception
import Control.Lens
import Data.Either
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Adventure.Engine
import Adventure.List.Utils

data AppEvent
  = AppInit
  | AppInputReceived -- TODO (james): This should be InputEntered
  | AppInputUpdated Text
  deriving (Eq, Show)

-- UI

buildUI
  :: WidgetEnv GameState AppEvent
  -> GameState
  -> WidgetNode GameState AppEvent
buildUI env model = widgetTree
  where
    renderedViewLabels = vstack $ intersperse spacer $ map renderedScene $ model ^. renderedViews
    rowSepColor = gray & L.a .~ 0.5
    renderedGameError err = label_ (T.pack . show $ err) [multiline] `styleBasic` []
    renderedErrorLabels = vstack $ intersperse spacer $ map renderedGameError $ model ^. gameErrors
    widgetTree = keystroke  [("Enter", AppInputReceived)] $ vstack
      [ scroll_ [] renderedViewLabels
      , textField_ inputBuffer [onChange AppInputUpdated]
      , scroll_ [] renderedErrorLabels `styleBasic` [height 28, padding 5]
      ] `styleBasic` [padding 10]

renderedScene :: Scene -> WidgetNode GameState AppEvent
renderedScene (Scene roomName roomDescription objects inventory exits) =
  vstack
  [ label_ roomName [] `styleBasic` [textFont "Noticia-Bold", textCenter]
  , separatorLine `styleBasic` [padding 10]
  , label_ roomDescription [] `styleBasic` [paddingL 20, paddingR 20, paddingB 10]
  , separatorLine `styleBasic` [padding 10]
  , hstack [ label_ "You see: " []
           , label_ (T.intercalate ", " $ map _gameObjectName objects) []
           ] `styleBasic` [paddingL 10, paddingR 10]
  , hstack [ label_ "You are holding: " []
           , label_ (T.intercalate ", " $ map _gameObjectName inventory) []
           ] `styleBasic` [paddingL 10, paddingR 10]
  , hstack [ label_ "Possible exits: " []
           , label_ (T.intercalate ", " $ map _exitName exits) []
           ] `styleBasic` [paddingL 10, paddingR 10]
  ]

handleEvent :: WidgetEnv GameState AppEvent
  -> WidgetNode GameState AppEvent
  -> GameState
  -> AppEvent
  -> [EventResponse GameState AppEvent GameState ()]
handleEvent env node model event =
  case event of
    AppInit -> []
    AppInputReceived -> [ Model $ updateGame model ]
    AppInputUpdated txt -> [ Model $ model & inputBuffer .~ txt ]

config
  = [ appWindowTitle "Adventure Engine"
    , appTheme darkTheme
    , appFontDef "Regular" "./assets/Alice-Regular.ttf"
    , appFontDef "Noticia-Regular" "./assets/NoticiaText-Regular.ttf"
    , appFontDef "Noticia-Bold" "./assets/NoticiaText-Bold.ttf"
    , appFontDef "Noticia-Italic" "./assets/NoticiaText-Italic.ttf"
    , appFontDef "Noticia-BoldItalic" "./assets/NoticiaText-BoldItalic.ttf"
    , appInitEvent AppInit
    ]

updateGame :: GameState -> GameState
updateGame g@(GameState vs w rvs input errors) =
  case handle' g w input of
    Left err -> updateGameErrors g err
    Right world' ->
      case render world' of
        Left renderErr -> updateGameErrors g renderErr
        Right rendered ->
          g & renderedViews .~ rendered : rvs
            & world .~ world'
            & inputBuffer .~ ""
  where
    updateGameErrors :: GameState -> GameError -> GameState
    updateGameErrors (GameState _ _ _ _ errs) e
      | length errs < 3 = g & gameErrors .~ e : errs
      | otherwise = g & gameErrors .~ prepend e errs

start :: IO ()
start = case initialGameState of
  Left err -> throw err
  Right initialState -> startApp initialState handleEvent buildUI config

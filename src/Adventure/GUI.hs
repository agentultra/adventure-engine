{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Adventure.GUI where

import Monomer hiding (keystroke)
import qualified Monomer.Lens as L

import Control.Exception
import Control.Lens
import Control.Monad.Except
import Data.Either
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Adventure.Engine
import Adventure.GUI.Widgets.Keystroke
import Adventure.List.Utils
import System.FilePath
import System.Directory

data AppEvent
  = AppInit
  | AppInputReceived -- TODO (james): This should be InputEntered
  | AppInputUpdated Text
  | AppShowLastMsg
  | AppGameSaved SaveFileResult
  deriving (Eq, Show)

-- UI

buildUI
  :: WidgetEnv GameState AppEvent
  -> GameState
  -> WidgetNode GameState AppEvent
buildUI env model = widgetTree
  where
    bottomMarker = spacer `nodeKey` "bottomMarker"
    renderedSceneLabels = intersperse spacer $ map renderedScene $ model ^. scenes
    renderedViewStack = vstack . reverse $ bottomMarker : renderedSceneLabels
    gameViewArea
      = hstack
      [ scroll_ [] renderedViewStack `nodeKey` "scrollLabels"
      , renderInventory $ getPlayerInventory model
      ]
    rowSepColor = gray & L.a .~ 0.5
    renderedGameError err = label_ (T.pack . show $ err) [multiline] `styleBasic` []
    renderedErrorLabels = vstack $ intersperse spacer $ map label $ model ^. gameErrors
    widgetTree = keystroke  [("Enter", AppInputReceived)] $ vstack
      [ gameViewArea
      , textField_ inputBuffer [onChange AppInputUpdated]
      , scroll_ [] renderedErrorLabels `styleBasic` [height 28, padding 5]
      ] `styleBasic` [padding 10]

renderedScene :: Scene -> WidgetNode GameState AppEvent
renderedScene (Scene roomName roomDescription objects exits msgs) =
  let renderedMsgs = vstack $ map label msgs
  in vstack
     [ label_ roomName [] `styleBasic` [textFont "Noticia-Bold", textCenter]
     , separatorLine `styleBasic` [padding 10]
     , label_ roomDescription [] `styleBasic` [paddingL 20, paddingR 20, paddingB 10]
     , renderedMsgs
     , separatorLine `styleBasic` [padding 10]
     , hstack [ label_ "You see: " []
              , label_ (T.intercalate ", " $ map _gameObjectName objects) []
              ] `styleBasic` [paddingL 10, paddingR 10]
     , hstack [ label_ "Possible exits: " []
              , label_ (T.intercalate ", " $ map _exitName exits) []
              ] `styleBasic` [paddingL 10, paddingR 10]
     ]

renderInventory :: [GameObject] -> WidgetNode GameState AppEvent
renderInventory objs =
  vstack $ [ label_ "You are holding: " [] ] ++ map renderInventoryObject objs
  where
    renderInventoryObject obj = label_ (_gameObjectName obj) []

handleEvent :: WidgetEnv GameState AppEvent
  -> WidgetNode GameState AppEvent
  -> GameState
  -> AppEvent
  -> [EventResponse GameState AppEvent GameState ()]
handleEvent env node model event =
  case event of
    AppInit -> []
    AppInputReceived ->
      case handleUserCommand model of
        Quit -> [ exitApplication ]
        SaveGame fname -> [ Model $ model & inputBuffer .~ ""
                          , Task $ AppGameSaved <$> doGameSave fname model
                          ]
        Update -> [ Model $ updateGame model,
                    Event AppShowLastMsg
                  ]
    AppInputUpdated txt -> [ Model $ model & inputBuffer .~ txt ]
    AppShowLastMsg -> scrollToNode env node "bottomMarker"
    AppGameSaved result ->
      let message = case result of
            FileNotSaved fpath -> "Failed to save file: " <> fpath
            FileSaved fpath -> "Saved: " <> fpath
      in [ Model $ model & gameErrors .~ message :  model ^. gameErrors ]

scrollToNode :: WidgetEnv GameState AppEvent
  -> WidgetNode GameState AppEvent
  -> WidgetKey
  -> [EventResponse GameState AppEvent GameState ()]
scrollToNode wenv node key = maybeToList $ makeMsg <$> childInfo where
  makeMsg info = Message "scrollLabels" (ScrollTo $ info ^. L.viewport)
  path = pathFromKey wenv key
  childInfo = path >>= findWidgetByPath wenv node

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

data SaveFileResult
  = FileSaved Text
  | FileNotSaved Text
  deriving (Eq, Show)

doGameSave :: FilePath -> GameState -> IO SaveFileResult
doGameSave fname gameState = do
  homeDirRoot <- getHomeDirectory
  let saveGamePath = homeDirRoot </> ".adventure-engine" </> "saves" </> fname
  result <- runExceptT . saveGameState saveGamePath $ gameState
  case result of
    Left _ ->
      -- TODO (james): add debug logging
      pure . FileNotSaved $ "Failed to save game: " <> T.pack fname
    Right (returnFpath, _) ->
      pure . FileSaved $ T.pack fname

start :: IO ()
start = do
  ensureDirectories
  case initialGameState of
    Left err -> throw err
    Right initialState -> startApp initialState handleEvent buildUI config

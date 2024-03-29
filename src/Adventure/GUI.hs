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
import Adventure.Engine.Rewards
import qualified Adventure.Engine.ImageData as I
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
  | AppGameLoaded LoadFileResult
  | AppQuit
  deriving (Eq, Show)

-- UI

buildUI
  :: WidgetEnv GameState AppEvent
  -> GameState
  -> WidgetNode GameState AppEvent
buildUI env model =
  case _gameStateIsGameEnd model of
    Nothing -> widgetTree
    Just (GameEndReward _ title gameEndText) ->
      keystroke [("Enter", AppQuit)] $
      vstack [ label_ title [] `styleBasic` [textCenter]
             , separatorLine `styleBasic` [padding 10]
             , label_ gameEndText [] `styleBasic` [textCenter]
             ]
  where
    bottomMarker = spacer `nodeKey` "bottomMarker"
    renderedSceneLabels = intersperse spacer $ map renderedScene $ model ^. scenes
    renderedViewStack = vstack . reverse $ bottomMarker : renderedSceneLabels
    gameViewArea
      = hstack
      [ zstack $ renderBackground ++ [ scroll_ [] renderedViewStack `nodeKey` "scrollLabels" ]
      , vstack $ renderScore ++ (renderInventory $ getPlayerInventory model)
      ]
    renderBackground
      = maybeToList
      $ imageFromImageData <$> getCurrentBackground model
    imageFromImageData (imgName, imgData)
      = imageMem_ imgName
        (imgData ^. I.raw)
        (Size (fromIntegral $ imgData ^. I.width) (fromIntegral $ imgData ^. I.height))
        []
    rowSepColor = gray & L.a .~ 0.5
    renderedGameError err = label_ (T.pack . show $ err) [multiline] `styleBasic` []
    renderedErrorLabels = vstack $ intersperse spacer $ map label $ model ^. gameErrors
    widgetTree = keystroke  [("Enter", AppInputReceived)] $ vstack
      [ gameViewArea
      , textField_ inputBuffer [onChange AppInputUpdated]
      , scroll_ [] renderedErrorLabels `styleBasic` [height 28, padding 5]
      ] `styleBasic` [padding 10]
    renderScore = [ label_ ("Score: " <> (T.pack $ show (score (model ^. eventLog) (model ^. rewards)))) [] ]

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

renderInventory :: [GameObject] -> [WidgetNode GameState AppEvent]
renderInventory objs =
  [ label_ "You are holding: " [] ] ++ map renderInventoryObject objs
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
        LoadGame fname -> [ Task $ AppGameLoaded <$> doGameLoad fname ]
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
    AppGameLoaded result ->
      case result of
        FileNotLoaded msg -> [ Model $ model & gameErrors .~ msg : model ^. gameErrors ]
        FileLoaded fname gamestate -> [ Model $ gamestate & gameErrors .~ ["Loaded: " <> fname] ]
    AppQuit -> [ exitApplication ]

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

data LoadFileResult
  = FileNotLoaded Text
  | FileLoaded Text GameState
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

doGameLoad :: FilePath -> IO LoadFileResult
doGameLoad fname = do
  homeDirRoot <- getHomeDirectory
  let saveGamePath = homeDirRoot </> ".adventure-engine" </> "saves" </> fname
  result <- runExceptT . loadGameState $ saveGamePath
  case result of
    Left _ ->
      -- TODO (james): add debug logging
      pure . FileNotLoaded $ "Failed to load: " <> T.pack fname
    Right gameState -> pure $ FileLoaded (T.pack fname) gameState

start :: IO ()
start = do
  ensureDirectories
  initState <- loadGameData
  startApp initState handleEvent buildUI config

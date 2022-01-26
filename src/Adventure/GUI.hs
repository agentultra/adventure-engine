{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Adventure.GUI where

import Monomer hiding (keystroke)
import qualified Monomer.Lens as L

import Control.Exception
import Control.Lens
import Data.Either
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Adventure.Engine
import Adventure.GUI.Widgets.Keystroke
import Adventure.List.Utils

data AppEvent
  = AppInit
  | AppInputReceived -- TODO (james): This should be InputEntered
  | AppInputUpdated Text
  | AppShowLastMsg
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
      , renderInventory $ tryRenderGameObject (getInventoryObjects $ model ^. world)
      ]
    rowSepColor = gray & L.a .~ 0.5
    renderedGameError err = label_ (T.pack . show $ err) [multiline] `styleBasic` []
    renderedErrorLabels = vstack $ intersperse spacer $ map (label . gameErrorText) $ model ^. gameErrors
    widgetTree = keystroke  [("Enter", AppInputReceived)] $ vstack
      [ gameViewArea
      , textField_ inputBuffer [onChange AppInputUpdated]
      , scroll_ [] renderedErrorLabels `styleBasic` [height 28, padding 5]
      ] `styleBasic` [padding 10]
    -- TODO (james): fix error handling, this is stinky
    tryRenderGameObject (Left err)   = error $ show err
    tryRenderGameObject (Right objs) = objs

renderedScene :: Scene -> WidgetNode GameState AppEvent
renderedScene (Scene roomName roomDescription objects exits) =
  vstack
  [ label_ roomName [] `styleBasic` [textFont "Noticia-Bold", textCenter]
  , separatorLine `styleBasic` [padding 10]
  , label_ roomDescription [] `styleBasic` [paddingL 20, paddingR 20, paddingB 10]
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
      let inputTxt = model ^. inputBuffer
      in if inputTxt == "$quit"
         then [ exitApplication ]
         else [ Model $ updateGame model,
               Event AppShowLastMsg
              ]
    AppInputUpdated txt -> [ Model $ model & inputBuffer .~ txt ]
    AppShowLastMsg -> scrollToNode env node "bottomMarker"

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

updateGame :: GameState -> GameState
updateGame g@(GameState vs w rvs input errors) =
  case handle' g w input of
    Left err -> updateGameErrors g err
    Right world' ->
      case render world' of
        Left renderErr -> updateGameErrors g renderErr
        Right rendered ->
          g & scenes .~ rendered : rvs
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

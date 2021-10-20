{-# LANGUAGE ScopedTypeVariables #-}
module Adventure.GUI where

import Monomer
import qualified Monomer.Lens as L

import Control.Lens
import Data.Either
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Adventure.Engine

data AppEvent
  = AppInit
  | AppInputReceived
  deriving (Eq, Show)

-- Widgets

inputField_ :: WidgetEvent e => Text -> [TextFieldCfg s e] -> WidgetNode s e
inputField_ value = textFieldD_ widgetData
  where
    widgetData = WidgetValue value

-- UI

buildUI
  :: WidgetEnv GameState AppEvent
  -> GameState
  -> WidgetNode GameState AppEvent
buildUI env model = widgetTree
  where
    renderedViewLabel txt = label_ txt [multiline] `styleBasic` [borderB 1 rowSepColor]
    renderedViewLabels = vstack $ intersperse spacer $ map renderedViewLabel $ model ^. renderedViews
    rowSepColor = gray & L.a .~ 0.5
    widgetTree = keystroke  [("Enter", AppInputReceived)] $ vstack
      [ scroll_ [] renderedViewLabels
      , inputField_ "" []
      ] `styleBasic` [padding 10]

handleEvent :: WidgetEnv GameState AppEvent
  -> WidgetNode GameState AppEvent
  -> GameState
  -> AppEvent
  -> [EventResponse GameState AppEvent GameState ()]
handleEvent env node model event =
  case event of
    AppInit -> []
    AppInputReceived -> [ Model $ model & renderedViews .~ "FOFOFOFOFOFOFOFO" : model ^. renderedViews
                        ]

config
  = [ appWindowTitle "Adventure Engine"
    , appTheme darkTheme
    , appFontDef "Regular" "./assets/Alice-Regular.ttf"
    , appInitEvent AppInit
    ]

start :: IO ()
start = startApp initialGameState handleEvent buildUI config

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
    renderedViewLabel txt = label_ txt [multiline] `styleBasic` [borderB 1 rowSepColor]
    renderedViewLabels = vstack $ intersperse spacer $ map renderedViewLabel $ model ^. renderedViews
    rowSepColor = gray & L.a .~ 0.5
    renderedGameError err = label_ (T.pack . show $ err) [multiline] `styleBasic` []
    renderedErrorLabels = vstack $ intersperse spacer $ map renderedGameError $ model ^. gameErrors
    widgetTree = keystroke  [("Enter", AppInputReceived)] $ vstack
      [ scroll_ [] renderedViewLabels
      , textField_ inputBuffer [onChange AppInputUpdated]
      , scroll_ [] renderedErrorLabels `styleBasic` [height 28, padding 5]
      ] `styleBasic` [padding 10]

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
start = startApp initialGameState handleEvent buildUI config

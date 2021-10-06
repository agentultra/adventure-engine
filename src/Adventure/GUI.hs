{-# LANGUAGE ScopedTypeVariables #-}
module Adventure.GUI where

import Monomer

import Control.Lens
import qualified Data.Text as T
import Adventure.Engine

data AppEvent = AppInit deriving (Eq, Show)

buildUI
  :: WidgetEnv GameState AppEvent
  -> GameState
  -> WidgetNode GameState AppEvent
buildUI env model = widgetTree
  where
    widgetTree = vstack
      [ label "Hello, world!"
      -- TODO (james): compose a lens to access the logMessage field of the World!
      , textArea testMessage
      ] `styleBasic` [padding 10]

handleEvent env node model event =
  case event of
    AppInit -> []

config
  = [ appWindowTitle "Adventure Engine"
    , appTheme darkTheme
    , appFontDef "Regular" "./assets/Alice-Regular.ttf"
    , appInitEvent AppInit
    ]

start :: IO ()
start = startApp defaultGameState handleEvent buildUI config

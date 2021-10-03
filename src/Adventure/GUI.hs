module Adventure.GUI where

import Monomer

import Adventure.Engine

data AppEvent = AppInit deriving (Eq, Show)

buildUI
  :: WidgetEnv World AppEvent
  -> World
  -> WidgetNode World AppEvent
buildUI env model = widgetTree
  where
    widgetTree = vstack
      [ label "Hello, world!"
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
start = startApp defaultWorld handleEvent buildUI config

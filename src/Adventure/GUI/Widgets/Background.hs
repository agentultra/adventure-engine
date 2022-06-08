{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Adventure.GUI.Widgets.Background where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Default
import Data.Text (Text)
import Monomer.Graphics.Types
import Monomer.Widgets.Single

import Debug.Trace

data BackgroundConfig
  = BackgroundConfig
  { _backgroundConfigImageName :: Text
  , _backgroundConfigImageSize :: Size
  , _backgroundConfigImageData :: ByteString
  }
  deriving (Eq, Show)

data BackgroundState = BackgroundState

makeFields ''BackgroundConfig

background :: BackgroundConfig -> WidgetNode s e
background backgroundConfig = defaultWidgetNode (WidgetType "background") widget where
  widget = makeBackground backgroundConfig BackgroundState

makeBackground :: BackgroundConfig -> BackgroundState -> Widget s e
makeBackground backgroundConfig backgroundState =
  createSingle backgroundState def
  { singleRender = render
  , singleGetSizeReq = getSizeReq
  }
  where
    getSizeReq wenv node = (expandSize 100 1, expandSize 100 1)
    render :: WidgetEnv s e -> WidgetNode s e -> Renderer -> IO ()
    render env node renderer = do
      addImage
        renderer
        (backgroundConfig ^. imageName)
        (backgroundConfig ^. imageSize)
        (backgroundConfig ^. imageData)
        []
      beginPath renderer
      setFillImagePattern renderer "test" (Point 0 0) (Size 1499 720) 0 1
      renderRect renderer $ Rect 0 0 360 480
      fill renderer

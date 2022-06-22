{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Adventure.Engine.ImageData where

import Control.Lens
import Data.ByteString (ByteString)

data ImageData
  = ImageData
  { _imageDataRaw    :: ByteString
  , _imageDataWidth  :: Int
  , _imageDataHeight :: Int
  }
  deriving (Eq, Show)

makeFields ''ImageData

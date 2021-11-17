{-# LANGUAGE OverloadedStrings #-}
module Adventure.GUI where

import Data.IORef
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.Hoppy.Runtime (withScopedPtr)
import Graphics.UI.Qtah.Signal (connect_)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout

import Adventure.Engine

run :: IO ()
run = withScopedPtr (QApplication.new [T.unpack ""]) $ \_ -> do
  gsRef <- newIORef initGameState
  window <- QWidget.new
  layout <- QVBoxLayout.newWithParent window
  inputArea <- QLineEdit.newWithParent window
  scene <- QTextEdit.new
  errorsList <- QTextEdit.new
  QTextEdit.setReadOnly errorsList True
  renderScene gsRef scene
  QTextEdit.setReadOnly scene True
  QBoxLayout.addWidget layout scene
  QBoxLayout.addWidget layout inputArea
  QBoxLayout.addWidget layout errorsList
  QWidget.setWindowTitle window $ T.unpack "Adventure Engine"
  QWidget.resizeRaw window 500 350

  connect_ inputArea QLineEdit.textChangedSignal $ \input ->
    modifyIORef' gsRef (\gs' -> gs' { _gameStateInputBuf = T.pack input })

  connect_ inputArea QLineEdit.returnPressedSignal $ do
    gs' <- readIORef gsRef
    case handle' gs' (_gameStateWorld gs') (_gameStateInputBuf gs') of
      Left err -> do
        updateGameState gsRef $ AddGameError err
        renderErrors gsRef errorsList
      Right world' ->
        case render world' of
          Left renderErr -> do
            updateGameState gsRef $ AddGameError renderErr
            renderErrors gsRef errorsList
          Right rendered -> do
            updateGameState gsRef (GameStateUpdate world' rendered)
            renderScene gsRef scene
            QLineEdit.clear inputArea

  QWidget.show window
  QCoreApplication.exec

renderScene :: IORef GameState -> QTextEdit.QTextEdit -> IO ()
renderScene gsRef scene = do
  gs <- readIORef gsRef
  QTextEdit.setPlainText scene $ T.unpack . concatScenes . _gameStateScenes $ gs
  where
    concatScenes :: [Text] -> Text
    concatScenes = T.unlines . intersperse "*************************\n"

renderErrors :: IORef GameState -> QTextEdit.QTextEdit -> IO ()
renderErrors gsRef textArea = do
  gs <- readIORef gsRef
  QTextEdit.setPlainText textArea $ unlines . map show . _gameStateErrors $ gs

data GameStateUpdate
  = GameStateUpdate World Text
  | AddGameError GameError

updateGameState :: IORef GameState -> GameStateUpdate -> IO ()
updateGameState gsRef (GameStateUpdate world renderedScene) =
  modifyIORef' gsRef (\gs -> gs { _gameStateScenes = renderedScene : _gameStateScenes gs
                                , _gameStateInputBuf = ""
                                , _gameStateWorld = world
                                })
updateGameState gsRef (AddGameError err) =
  modifyIORef' gsRef (\gs -> gs { _gameStateErrors = take 3 $ err : _gameStateErrors gs })

module Adventure.GUI where

import qualified Data.Text as T
import Foreign.Hoppy.Runtime (withScopedPtr)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QBoxLayout as QBoxLayout
import qualified Graphics.UI.Qtah.Core.QCoreApplication as QCoreApplication
--import qualified Graphics.UI.Qtah.Widgets.QLabel as QLabel
import qualified Graphics.UI.Qtah.Widgets.QLineEdit as QLineEdit
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout

import Adventure.Engine

run :: IO ()
run = withScopedPtr (QApplication.new [""]) $ \_ -> do
  let gs = initGameState
  window <- QWidget.new
  layout <- QVBoxLayout.newWithParent window
  inputArea <- QLineEdit.newWithParent window
  scene <- QTextEdit.newWithText $ T.unpack . T.unlines . _gameStateScenes $ gs
  QTextEdit.setReadOnly scene True
  QBoxLayout.addWidget layout scene
  QBoxLayout.addWidget layout inputArea
  QWidget.setWindowTitle window "Adventure Engine"
  QWidget.resizeRaw window 500 350
  QWidget.show window
  QCoreApplication.exec

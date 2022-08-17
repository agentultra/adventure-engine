module Maker where

import qualified Data.Aeson as JSON
import qualified Data.Text.IO as T
import Maker.Parser
import System.Directory
import System.Environment
import System.FilePath
import Text.Megaparsec

helpText :: String
helpText = "Usage: adventure-maker DIR"

run :: IO ()
run = do
  args <- getArgs
  case args of
    [path] -> processPath path
    _      -> putStrLn helpText

processPath :: FilePath -> IO ()
processPath path = do
  let worldTxtPath = path </> "world.txt"
      outputPath = path </> "build"
  createDirectoryIfMissing True outputPath
  worldTxt <- T.readFile worldTxtPath
  let worldDataResult = runParser worldParser worldTxtPath worldTxt
  case worldDataResult of
    Left err -> putStrLn . errorBundlePretty $ err
    Right worldData ->
      JSON.encodeFile (outputPath </> "world.json") worldData

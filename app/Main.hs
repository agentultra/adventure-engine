module Main where

import qualified Data.Text.IO as T

import Adventure.Engine

main :: IO ()
main = do
  case render defaultWorld of
    Left error -> print $ show error
    Right out  -> T.putStrLn out

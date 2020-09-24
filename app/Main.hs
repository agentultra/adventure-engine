module Main where

import Adventure.Engine

main :: IO ()
main = do
  case render defaultWorld of
    Left error -> print $ show error
    Right out  -> putStrLn out

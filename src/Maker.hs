module Maker where

import System.Environment

run :: IO ()
run = do
  args <- getArgs
  print args

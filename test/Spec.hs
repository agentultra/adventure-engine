{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Text (Text)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Adventure.Engine

main :: IO ()
main = hspec $
  describe "saveGameState" $
    context "given the defaultGameState" $ do
      it "should write save files to the engine's path in the users' directory" $ do
        result <- (execWriterT . runExceptT . saveGameState "foo" $ defaultGameState)
        (fst . head $ (result :: [(FilePath, Text)]))
          `shouldBe` ("~/.adventure-engine/saves/foo" :: FilePath)

      it "should restore default state from save file" $ do
        fileContents <- execWriterT . runExceptT . saveGameState "foo" $ defaultGameState
        loadedGameState <- (`evalStateT` (fileContents :: [(FilePath, Text)])) . runExceptT . loadGameState $ "foo"
        Right defaultGameState `shouldBe` loadedGameState

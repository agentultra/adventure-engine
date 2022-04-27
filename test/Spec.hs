{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec

import Data.Text (Text)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

import Adventure.Engine
import Adventure.Engine.Rewards

main :: IO ()
main = hspec $ do
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

  fdescribe "score" $ do
    context "given some duplicate event in the event log" $
      it "should count only the first event" $ do
        let events = [ItemPickedUp 5, ItemPickedUp 5, ItemDropped]
            rewards = [EventReward (ItemPickedUp 5) 100]
        score events rewards `shouldBe` 100

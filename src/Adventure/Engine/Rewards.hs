{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Adventure.Engine.Rewards where

import Data.Aeson
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics

import Adventure.Engine.Database
import Adventure.Engine.Objects

data Event
  = ItemPickedUp (EntityId GameObject)
  | ItemDropped (EntityId GameObject)
  | ItemExamined (EntityId GameObject)
  | ItemTakenFromContainer (EntityId GameObject) (EntityId GameObject)
  -- ^ Player took ITEM from CONTAINER
  | PlayerMoved (EntityId Room) (EntityId Room)
  -- ^ Player moved FROM room TO room
  | PlayerLookedInContainer (EntityId GameObject)
  | PlayerLookedAtExit (EntityId Exit)
  | Dug (EntityId Room) (Maybe (EntityId GameObject))
  | ContainerUnlocked (EntityId GameObject) (EntityId GameObject)
  -- ^ Player attempted to unlock CONTAINER with ITEM
  | ContainerUnlockFailed (EntityId GameObject) (EntityId GameObject)
  -- ^ Player attempted to unlock CONTAINER with ITEM
  deriving (Eq, Generic, Ord, Show)

deriving instance ToJSON Event
deriving instance FromJSON Event

data EventReward
  = EventReward
  { _eventRewardEvent   :: Event
  , _eventRewardAmount  :: Int
  }
  deriving (Eq, Generic, Show)

deriving instance ToJSON EventReward
deriving instance FromJSON EventReward

data Reward
  = Awarded EventReward
  | NoAward
  deriving (Eq, Generic, Show)

reward :: Event -> EventReward -> Reward
reward event r@(EventReward rewardEvent _)
  | event == rewardEvent = Awarded r
  | otherwise            = NoAward

score :: [Event] -> [EventReward] -> Int
score _ [] = 0
score events eventRewards = go (mempty, 0) events eventRewards
  where
    go :: (Set Event, Int) -> [Event] -> [EventReward] -> Int
    go (_, scr) _ [] = scr
    go scr evts (eventReward:rest) =
      let scr' = foldl' sumReward (mempty, 0) . map (`reward` eventReward) $ events
      in go (scr `add` scr') evts rest

    sumReward :: (Set Event, Int) -> Reward -> (Set Event, Int)
    sumReward (scoredEvents, scr) (Awarded (EventReward evt amt)) =
      if evt `S.member` scoredEvents
      then (scoredEvents, amt)
      else (S.insert evt scoredEvents, amt + scr)
    sumReward scr _             = scr

    add :: (Set Event, Int) -> (Set Event, Int) -> (Set Event, Int)
    add (levts, lamt) (revts, ramt) = (levts <> revts, lamt + ramt)

data GameEndReward
  = GameEndReward
  { gameEndEvent :: Event
  , gameEndTitle :: Text
  , gameEndText  :: Text
  }
  deriving (Eq, Generic, Show)

deriving instance ToJSON GameEndReward
deriving instance FromJSON GameEndReward

gameEnd :: Event -> NonEmpty GameEndReward -> Maybe GameEndReward
gameEnd event (endEvent :| rest)
  | event == gameEndEvent endEvent = Just endEvent
  | otherwise = find (checkGameEnd event) rest
  where
    checkGameEnd :: Event -> GameEndReward -> Bool
    checkGameEnd ev (GameEndReward rewardEvent _ _) = ev == rewardEvent

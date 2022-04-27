{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Adventure.Engine.Rewards where

import Data.Aeson
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics

import Adventure.Engine.Database
import Adventure.Engine.Objects

data Event
  = ItemPickedUp (EntityId GameObject)
  | ItemDropped
  deriving (Eq, Generic, Ord, Show)

deriving instance ToJSON Event
deriving instance FromJSON Event

data EventReward
  = EventReward
  { _eventRewardEvent  :: Event
  , _eventRewardAmount :: Int
  }
  deriving (Eq, Generic, Show)

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

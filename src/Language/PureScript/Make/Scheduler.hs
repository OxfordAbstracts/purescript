module Language.PureScript.Make.Scheduler
  ( Scheduler
  , Priority
  , newScheduler
  , acquire
  , release
  , criticalPathPriorities
  ) where

import Prelude

import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (mask_, onException, uninterruptibleMask_)
import Control.Monad (unless)
import Data.Foldable (foldl', for_)
import Data.Map.Strict qualified as M
import Data.Ord (Down(..))

type Priority = Int

type Ticket = (Down Priority, Int)

data Slots = Slots
  { slotsFree :: !Int
  , slotsNextArrival :: !Int
  , slotsWaiting :: !(M.Map Ticket (MVar ()))
  }

newtype Scheduler = Scheduler (MVar Slots)

newScheduler :: Int -> IO Scheduler
newScheduler slotCount = Scheduler <$> newMVar (Slots slotCount 0 M.empty)

acquire :: Scheduler -> Priority -> IO ()
acquire scheduler@(Scheduler var) priority = mask_ $ do
  queued <- modifyMVar var $ \slots ->
    if slotsFree slots > 0
      then pure (slots { slotsFree = slotsFree slots - 1 }, Nothing)
      else do
        granted <- newEmptyMVar
        let ticket = (Down priority, slotsNextArrival slots)
        pure
          ( slots
              { slotsNextArrival = slotsNextArrival slots + 1
              , slotsWaiting = M.insert ticket granted (slotsWaiting slots)
              }
          , Just (ticket, granted)
          )
  for_ queued $ \(ticket, granted) ->
    takeMVar granted `onException` withdraw ticket
  where
  withdraw :: Ticket -> IO ()
  withdraw ticket = uninterruptibleMask_ $ do
    wasStillWaiting <- modifyMVar var $ \slots ->
      pure (slots { slotsWaiting = M.delete ticket (slotsWaiting slots) }, M.member ticket (slotsWaiting slots))
    unless wasStillWaiting $ release scheduler

release :: Scheduler -> IO ()
release (Scheduler var) = uninterruptibleMask_ $ do
  next <- modifyMVar var $ \slots ->
    pure $ case M.minView (slotsWaiting slots) of
      Nothing -> (slots { slotsFree = slotsFree slots + 1 }, Nothing)
      Just (granted, stillWaiting) -> (slots { slotsWaiting = stillWaiting }, Just granted)
  for_ next (`putMVar` ())

criticalPathPriorities :: forall name. Ord name => [(name, [name], Priority)] -> M.Map name Priority
criticalPathPriorities dependenciesFirst =
  fst $ foldl' visit (M.empty, M.empty) (reverse dependenciesFirst)
  where
  visit :: (M.Map name Priority, M.Map name Priority) -> (name, [name], Priority) -> (M.Map name Priority, M.Map name Priority)
  visit (!priorities, !longestDownstream) (name, dependencies, cost) =
    let priority = cost + M.findWithDefault 0 name longestDownstream
    in ( M.insert name priority priorities
       , foldl' (\downstream dependency -> M.insertWith max dependency priority downstream) longestDownstream dependencies
       )

module FRPEngine.Input.Input where

import Data.Maybe
import FRPEngine.Input.Internal.CustomEventLift
import FRPEngine.Input.Internal.UpdateKeys
import FRPEngine.Input.Types
import SDL

updateInput :: [Input] -> [SDL.Event] -> [Input]
updateInput inputState events =
  foldr (flip updateKeys) keys' (liftCustomEvent events')
  where
    keys' = flushKeys inputState
    events' = catMaybes $ (filterOutRepeatingEvents . eventPayload) <$> events
    -- The events that have repeating set to true aren't needed
    filterOutRepeatingEvents (KeyboardEvent (KeyboardEventData _ _ True _)) =
      Nothing
    filterOutRepeatingEvents a =
      Just a

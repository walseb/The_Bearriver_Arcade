module FRPEngine.Input.Input where

import Data.Maybe
import FRPEngine.Input.Internal.CustomEventLift
import FRPEngine.Input.Internal.UpdateKeys
import FRPEngine.Input.Types
import SDL
import Control.DeepSeq

updateInput :: [Input] -> [SDL.Event] -> [Input]
updateInput inputState events =
  -- This needs to be deeply evaluated because otherwise thunks build up when keys aren't pressed
  deepseq (foldr (flip updateKeys) keys' (liftCustomEvent events')) (foldr (flip updateKeys) keys' (liftCustomEvent events'))
  where
    keys' = flushKeys inputState
    events' = catMaybes $ (filterOutRepeatingEvents . eventPayload) <$> events
    -- The events that have repeating set to true aren't needed
    filterOutRepeatingEvents (KeyboardEvent (KeyboardEventData _ _ True _)) =
      Nothing
    filterOutRepeatingEvents a =
      Just a

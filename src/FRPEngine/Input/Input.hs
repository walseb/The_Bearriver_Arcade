module FRPEngine.Input.Input (inputStateUpdate) where

import Data.Maybe
import FRPEngine.Input.Internal.CustomEventConversion
import FRPEngine.Input.Internal.UpdateInputstate
import FRPEngine.Input.Types
import SDL

inputStateUpdate :: InputState -> [SDL.Event] -> InputState
inputStateUpdate inputState events =
  foldr (flip updateKeyInInputState) inputState' (toCustomEvent events')
  where
    inputState' = flushKeystate' inputState
    events' = catMaybes $ filterOutRepeatingEvents <$> (eventPayload <$> (debug <$> events))

debug a = a

filterOutRepeatingEvents (KeyboardEvent (KeyboardEventData _ _ True _)) =
  Nothing
filterOutRepeatingEvents a =
  Just a

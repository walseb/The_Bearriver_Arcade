module FRPEngine.Input.Input (inputStateUpdate) where

import SDL
import FRPEngine.Input.Internal
import FRPEngine.Input.Types

inputStateUpdate :: InputState -> [SDL.Event] -> InputState
inputStateUpdate inputState events =
  foldr updateKeyInInputState inputState (eventToCustomEventPayload events)

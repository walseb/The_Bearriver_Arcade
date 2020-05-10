module FRPEngine.Input.Input where

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
    events' = catMaybes $ (filterOutRepeatingEvents . eventPayload) <$> events
    -- The events that have repeating set to true aren't needed
    filterOutRepeatingEvents (KeyboardEvent (KeyboardEventData _ _ True _)) =
      Nothing
    filterOutRepeatingEvents a =
      Just a

defaultKeybinds =
  InputState
    { _zoom =
        (ScrollState 0),
      -- (ButtonAxisState (V2 KeycodeB KeycodeO) (V2 False False)),
      _movement =
        DirectionalInput
          (ButtonState KeycodeUp False 0)
          (ButtonState KeycodeDown False 0)
          (ButtonState KeycodeLeft False 0)
          (ButtonState KeycodeRight False 0),
          -- (ButtonState KeycodeM False 0)
          -- (ButtonState KeycodeT False 0)
          -- (ButtonState KeycodeS False 0)
          -- (ButtonState KeycodeN False 0),
      _quit = ButtonState KeycodeEscape False 0
      -- _quit = ButtonState KeycodeQ False 0
    }

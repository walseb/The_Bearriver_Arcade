module FRPEngine.Input.Internal where

import Control.Lens
import SDL
import FRPEngine.Input.Types

data CustomEventPayload
  = -- SDL doesn't send a scroll stopped event, therefor we need to create our own
  ScrollStopped
  | SDLEvent {_payload :: EventPayload}

eventToCustomEventPayload :: [Event] -> [CustomEventPayload]
eventToCustomEventPayload event =
  fmap SDLEvent payload
    -- Add on the scroll stopped event if there aren't any mouse scroll events
    ++ case (or (fmap isMouseWheel payload)) of
      True -> []
      False -> [ScrollStopped]
  where
    isMouseWheel (MouseWheelEvent _) = True
    isMouseWheel _ = False
    payload = fmap eventPayload event

fromMotion :: InputMotion -> Bool
fromMotion Pressed = True
fromMotion Released = False

-- This gets
replaceKeystate :: KeyState -> CustomEventPayload -> KeyState
-- ButtonState
replaceKeystate a@(ButtonState key _) (SDLEvent (KeyboardEvent (KeyboardEventData _ pressed _ keysym))) =
  if keysymKeycode keysym == key then ButtonState (keysymKeycode keysym) (fromMotion pressed) else a
-- ButtonAxisState
replaceKeystate a@(ButtonAxisState key press) (SDLEvent (KeyboardEvent (KeyboardEventData _ press0 _ keysym)))
  | keysymKeycode keysym == (key ^. _x) = ButtonAxisState key (V2 (press ^. _x) (fromMotion press0))
  | keysymKeycode keysym == (key ^. _y) = ButtonAxisState key (V2 (fromMotion press0) (press ^. _y))
  | otherwise = a
-- ScrollState
replaceKeystate (ScrollState oldScroll) (SDLEvent (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollNormal))) =
  ScrollState $ oldScroll + fromIntegral (scrollDist ^. _y)
replaceKeystate (ScrollState oldScroll) (SDLEvent (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollFlipped))) =
  ScrollState $ oldScroll + (- (fromIntegral (scrollDist ^. _y)))
replaceKeystate (ScrollState _) ScrollStopped =
  ScrollState 0
-- If keys aren't matched or unknown don't do anything to them
replaceKeystate a _ = a

-- We create a new ScrollState every update because there isn't a SDL event for "I have stopped scrolling now"
updateKeyInInputState :: InputState -> CustomEventPayload -> InputState
updateKeyInInputState (InputState b1 (DirectionalInput b2 b3 b4 b5) bQuit) event =
  InputState
    (rk b1)
    ( DirectionalInput
        (rk b2)
        (rk b3)
        (rk b4)
        (rk b5)
    )
    (rk bQuit)
  where
    rk = (flip replaceKeystate) event

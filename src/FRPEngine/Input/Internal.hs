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
replaceKeystate :: CustomEventPayload -> KeyState -> KeyState
-- ButtonState
replaceKeystate (SDLEvent (KeyboardEvent (KeyboardEventData _ pressed _ keysym))) a@(ButtonState key _) = if keysymKeycode keysym == key then ButtonState (keysymKeycode keysym) (fromMotion pressed) else a
-- ButtonAxisState
replaceKeystate (SDLEvent (KeyboardEvent (KeyboardEventData _ press0 _ keysym))) a@(ButtonAxisState key press)
  | keysymKeycode keysym == (key ^. _x) = ButtonAxisState key (V2 (press ^. _x) (fromMotion press0))
  | keysymKeycode keysym == (key ^. _y) = ButtonAxisState key (V2 (fromMotion press0) (press ^. _y))
  | otherwise = a
-- ScrollState
replaceKeystate (SDLEvent (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollNormal))) (ScrollState oldScroll) = ScrollState $ oldScroll + fromIntegral (scrollDist ^. _y)
replaceKeystate (SDLEvent (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollFlipped))) (ScrollState oldScroll) = ScrollState $ oldScroll + (- (fromIntegral (scrollDist ^. _y)))
replaceKeystate ScrollStopped (ScrollState _) = ScrollState 0
-- If keys aren't matched or unknown don't do anything to them
replaceKeystate _ a = a

-- We create a new ScrollState every update because there isn't a SDL event for "I have stopped scrolling now"
updateKeyInInputState :: CustomEventPayload -> InputState-> InputState
updateKeyInInputState event (InputState b1 (DirectionalInput b2 b3 b4 b5) bQuit) =
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
    rk = replaceKeystate event

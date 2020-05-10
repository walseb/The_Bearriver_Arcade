module FRPEngine.Input.Internal.UpdateInputstate where

import Control.Lens
import FRPEngine.Input.Internal.Types
import FRPEngine.Input.Types
import SDL

fromMotion :: InputMotion -> Bool
fromMotion Pressed = True
fromMotion Released = False

replaceKeystate :: KeyState -> CustomEventPayload -> KeyState
-- ButtonState
replaceKeystate a@(ButtonState k _ pr) (SDLEvent (KeyboardEvent (KeyboardEventData _ press _ keysym))) =
  case k == keysymKeycode keysym of
    True -> ButtonState (keysymKeycode keysym) (fromMotion press) pr
    False -> a
-- EventPR
replaceKeystate a@(ButtonState k press pr) (EventPR (KeyboardEvent (KeyboardEventData _ _ _ keysym))) =
  case k == keysymKeycode keysym of
    True -> (ButtonState k press (pr + 1))
    False -> a
-- ButtonAxisState
replaceKeystate a@(ButtonAxisState k press) (SDLEvent (KeyboardEvent (KeyboardEventData _ press0 _ keysym)))
  | keysymKeycode keysym == (k ^. _x) = ButtonAxisState k (V2 (press ^. _x) (fromMotion press0))
  | keysymKeycode keysym == (k ^. _y) = ButtonAxisState k (V2 (fromMotion press0) (press ^. _y))
  | otherwise = a
-- ScrollState
replaceKeystate (ScrollState oldScroll) (SDLEvent (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollNormal))) =
  ScrollState $ oldScroll + fromIntegral (scrollDist ^. _y)
replaceKeystate (ScrollState oldScroll) (SDLEvent (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollFlipped))) =
  ScrollState $ oldScroll + (- (fromIntegral (scrollDist ^. _y)))
-- If keys aren't matched or unknown don't do anything to them
replaceKeystate a _ = a

-- This resets discrete input events (events that don't produce an end event) like scroll
flushKeystate :: KeyState -> KeyState
flushKeystate (ScrollState _) = ScrollState 0
flushKeystate (ButtonState k press _) = ButtonState k press 0
flushKeystate a = a

flushKeystate' :: InputState -> InputState
flushKeystate' i =
  updateInputState i flushKeystate

-- We create a new ScrollState every update because there isn't a SDL event for "I have stopped scrolling now"
updateKeyInInputState :: InputState -> CustomEventPayload -> InputState
updateKeyInInputState i event =
  updateInputState i ((flip replaceKeystate) event)

updateInputState (InputState b1 (DirectionalInput b2 b3 b4 b5) bQuit) func =
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
    rk = func

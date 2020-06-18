module FRPEngine.Input.Internal.UpdateKeys (updateKeys, flushKeys) where

import Control.Lens
import FRPEngine.Input.Internal.Types
import FRPEngine.Input.Types
import SDL

fromMotion :: InputMotion -> Bool
fromMotion Pressed = True
fromMotion Released = False

updateKey :: Input -> CustomEventPayload -> Input
--InpBtn
updateKey a@(InpBtn pr (Btn k _)) (SDLEvent (KeyboardEvent (KeyboardEventData _ press _ keysym))) =
  case k == keysymKeycode keysym of
    True -> InpBtn pr (Btn (keysymKeycode keysym) (fromMotion press))
    False -> a
-- EventPR
updateKey a@(InpBtn pr (Btn k press)) (EventPR (KeyboardEvent (KeyboardEventData _ _ _ keysym))) =
  case k == keysymKeycode keysym of
    True -> (InpBtn (pr + 1) (Btn k press))
    False -> a
--InpBtn2D
updateKey a@(InpBtn2D b@(V2 (Btn k1 _) (Btn k2 _))) (SDLEvent (KeyboardEvent (KeyboardEventData _ press0 _ keysym)))
  | keysymKeycode keysym == k1 = InpBtn2D $ (_x . pressed) .~ fromMotion press0 $ b
  | keysymKeycode keysym == k2 = InpBtn2D $ (_y . pressed) .~ fromMotion press0 $ b
  | otherwise = a
--InpBtn4D
updateKey a@(InpBtn4D b@(V4 (Btn k1 _) (Btn k2 _) (Btn k3 _) (Btn k4 _))) (SDLEvent (KeyboardEvent (KeyboardEventData _ press0 _ keysym)))
  | keysymKeycode keysym == k1 = InpBtn4D $ (_x . pressed) .~ fromMotion press0 $ b
  | keysymKeycode keysym == k2 = InpBtn4D $ (_y . pressed) .~ fromMotion press0 $ b
  | keysymKeycode keysym == k3 = InpBtn4D $ (_z . pressed) .~ fromMotion press0 $ b
  | keysymKeycode keysym == k4 = InpBtn4D $ (_w . pressed) .~ fromMotion press0 $ b
  | otherwise = a
--InpScroll
updateKey (InpScroll oldScroll) (SDLEvent (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollNormal))) =
  InpScroll $ oldScroll + fromIntegral (scrollDist ^. _y)
updateKey (InpScroll oldScroll) (SDLEvent (MouseWheelEvent (MouseWheelEventData _ _ scrollDist ScrollFlipped))) =
  InpScroll $ oldScroll + (- (fromIntegral (scrollDist ^. _y)))
-- Close window
updateKey (InpCloseWindow False) (SDLEvent (WindowClosedEvent _)) = InpCloseWindow True
-- If keys aren't matched or unknown don't do anything to them
updateKey a _ = a

flushKeys :: [Input] -> [Input]
flushKeys i =
  flush <$> i
  where
    -- This resets discrete input events (events that don't produce an end event) like scroll
    flush :: Input -> Input
    flush (InpScroll _) = InpScroll 0
    flush (InpBtn _ (Btn k press)) = InpBtn 0 (Btn k press)
    flush a = a

-- We create a new InpScroll every update because there isn't a SDL event for "I have stopped scrolling now"
updateKeys :: [Input] -> CustomEventPayload -> [Input]
updateKeys i event =
  flip updateKey event <$> i

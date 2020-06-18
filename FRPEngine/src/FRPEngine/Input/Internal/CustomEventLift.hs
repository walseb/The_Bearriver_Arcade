module FRPEngine.Input.Internal.CustomEventLift (liftCustomEvent) where

import FRPEngine.Input.Internal.Types
import SDL

liftCustomEvent :: [EventPayload] -> [CustomEventPayload]
liftCustomEvent e =
  lift e []
  where
    lift :: [EventPayload] -> [CustomEventPayload] -> [CustomEventPayload]
    lift [] result =
      result
    lift (e' : es) result =
      lift es $ sumEvents result [] (Just e')

-- This adds up events as much as possible. For example scrolling two times are added up to one big scroll. This makes the implementation of the interpreted easier
sumEvents :: [CustomEventPayload] -> [CustomEventPayload] -> Maybe EventPayload -> [CustomEventPayload]
-- Sum key presses
sumEvents (a@(SDLEvent a'@(KeyboardEvent (KeyboardEventData _ pressed _ key))) : as) bs c@(Just (KeyboardEvent (KeyboardEventData _ pressed' _ key'))) =
  case (key == key' && pressed /= pressed') of
    -- If key has been pressed, unpressed and then pressed again OR unpressed, pressed and then pressed again between a frame, create accumulator
    True -> sumEvents as (EventPR a' : bs) Nothing
    False -> sumEvents as (a : bs) c
-- Sum scroll events
sumEvents (SDLEvent (MouseWheelEvent (MouseWheelEventData x y scroll1 dir)) : as) bs (Just (MouseWheelEvent (MouseWheelEventData _ _ scroll2 dir'))) =
  sumEvents as (SDLEvent (MouseWheelEvent (MouseWheelEventData x y scroll3 dir)) : bs) Nothing
  where
    scrollDirApply :: (Num a) => V2 a -> MouseScrollDirection -> V2 a
    scrollDirApply s ScrollNormal = s
    scrollDirApply s ScrollFlipped = -s
    scrollAdd s s' = scrollDirApply s dir + scrollDirApply s' dir'
    scroll3 = scrollAdd scroll1 scroll2
-- If we are out of keys, return
sumEvents [] bs (Just a) =
  SDLEvent a : bs
-- If key is already used up, we can just return
sumEvents as bs Nothing =
  as ++ bs
-- the current a isn't matching. For example if scroll and keyboard, just move on
sumEvents (a : as) bs c@(Just _) =
  sumEvents as (a : bs) c

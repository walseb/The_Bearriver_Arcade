module FRPEngine.Input.Internal.CustomEventConversion (toCustomEvent) where

import FRPEngine.Input.Internal.Types
import SDL

toCustomEvent :: [EventPayload] -> [CustomEventPayload]
toCustomEvent e =
  toCustomEvent' e []

toCustomEvent' :: [EventPayload] -> [CustomEventPayload] -> [CustomEventPayload]
toCustomEvent' [] result =
  result
toCustomEvent' (e : es) result =
  toCustomEvent' es $ sumEvents result [] (Just e)

sumEvents :: [CustomEventPayload] -> [CustomEventPayload] -> Maybe EventPayload -> [CustomEventPayload]
sumEvents (a@(SDLEvent a'@(KeyboardEvent (KeyboardEventData _ pressed _ key))) : as) bs c@(Just (KeyboardEvent (KeyboardEventData _ pressed' _ key'))) =
  case (key == key' && pressed /= pressed') of
    -- If key has been pressed, unpressed and then pressed again OR unpressed, pressed and then pressed again between a frame, create accumulator
    True -> sumEvents as (EventPR a' : bs) Nothing
    False -> sumEvents as (a : bs) c
sumEvents (SDLEvent (MouseWheelEvent (MouseWheelEventData x y scroll1 dir)) : as) bs (Just (MouseWheelEvent (MouseWheelEventData _ _ scroll2 dir'))) =
  sumEvents as (SDLEvent (MouseWheelEvent (MouseWheelEventData x y scroll3 dir)) : bs) Nothing
  where
    scrollFix :: (Num a) => V2 a -> MouseScrollDirection -> V2 a
    scrollFix s ScrollNormal = s
    scrollFix s ScrollFlipped = - s
    scrollAdd s s' = scrollFix s dir + scrollFix s' dir'
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

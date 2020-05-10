module FRPEngine.Input.Internal.Types where

import SDL

data CustomEventPayload
  = -- SDL doesn't send a scroll stopped event, therefor we need to create our own
    SDLEvent {_payload :: EventPayload}
    -- pr means "pressed then released" Meaning an event has been pressed and then released within the same frame. Without this that would result in nothing happening. So it shouldn't be used for stuff like movement because it doesn't matter if movement is pressed between frames, but for stuff like gun triggers this is pretty essential
    -- The count is summed up when folding these events into the input state
  | EventPR {_prPayload :: EventPayload}
  deriving (Show)

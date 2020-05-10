module FRPEngine.Input.Internal.Types where

import SDL

data CustomEventPayload
  = -- SDL doesn't send a scroll stopped event, therefor we need to create our own
    SDLEvent {_payload :: EventPayload}
    -- Meaning an even has been pressed and then released within the same frame. So it should be ignored except for binds that wait for a click
    -- This can't be stacked so it's summed up when turning it into the input state
  | EventPR {_prPayload :: EventPayload}
  deriving (Show)

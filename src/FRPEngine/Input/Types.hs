{-# LANGUAGE TemplateHaskell #-}

module FRPEngine.Input.Types where

import Control.Lens
import SDL

data KeyState
  = -- pr here means press release. Check FRPEngine.Input.Internal.Types for more info.
    ButtonState {_key :: !Keycode, _pressed :: !Bool, _prCount :: !Int}
  | ButtonAxisState {_keyVec :: !(V2 Keycode), _pressedVec :: !(V2 Bool)}
  | ScrollState {_scrollDist :: !Int}
  deriving (Show)

makeLenses ''KeyState
makePrisms ''KeyState

data DirectionalInput
  = DirectionalInput
      { _up :: !KeyState,
        _down :: !KeyState,
        _left :: !KeyState,
        _right :: !KeyState
      }
  deriving (Show)

makeLenses ''DirectionalInput

data InputState
  = InputState
      { _zoom :: !KeyState,
        _movement :: !DirectionalInput,
        _quit :: !KeyState
      }
  deriving (Show)

makeLenses ''InputState

makePrisms ''InputState

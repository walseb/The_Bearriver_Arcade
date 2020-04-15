{-# LANGUAGE Arrows #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}

module FRPEngine.Input.Types where

import Control.Lens
import SDL

data KeyState
  = -- pr here means press release. Check FRPEngine.Input.Internal.Types for more info.
    ButtonState {_key :: Keycode, _pressed :: Bool, _prCount :: Int}
  | ButtonAxisState {_keyVec :: V2 Keycode, _pressedVec :: V2 Bool}
  | ScrollState {_scrollDist :: Int}
  deriving (Show)

makeLenses ''KeyState

makePrisms ''KeyState

data DirectionalInput
  = DirectionalInput
      { _up :: KeyState,
        _down :: KeyState,
        _left :: KeyState,
        _right :: KeyState
      }
  deriving (Show)

makeLenses ''DirectionalInput

data InputState
  = InputState
      { _zoom :: KeyState,
        _movement :: DirectionalInput,
        _quit :: KeyState
      }
  deriving (Show)

makeLenses ''InputState

makePrisms ''InputState

defaultKeybinds =
  InputState
    { _zoom =
        (ScrollState 0),
      -- (ButtonAxisState (V2 KeycodeB KeycodeO) (V2 False False)),
      _movement =
        DirectionalInput
          -- (ButtonState KeycodeUp False)
          -- (ButtonState KeycodeDown False)
          -- (ButtonState KeycodeLeft False)
          -- (ButtonState KeycodeRight False)
          (ButtonState KeycodeM False 0)
          (ButtonState KeycodeT False 0)
          (ButtonState KeycodeS False 0)
          (ButtonState KeycodeN False 0),
      -- _quit = ButtonState KeycodeEscape False
      _quit = ButtonState KeycodeQ False 0
    }

{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module FRPEngine.Input.Types where

import Control.Lens
import SDL

data KeyState
  = ButtonState {_key :: Keycode, _pressed :: Bool}
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

keybinds =
  InputState
    { _zoom =
        -- (ScrollState 0),
      (ButtonAxisState (V2 KeycodeB KeycodeO) (V2 False False)),
      _movement =
        DirectionalInput
          (ButtonState KeycodeM False)
          (ButtonState KeycodeT False)
          (ButtonState KeycodeS False)
          (ButtonState KeycodeN False),
      _quit = ButtonState KeycodeQ False
    }

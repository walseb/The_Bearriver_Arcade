{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

-- This prevents a space memory leak from happening
{-# LANGUAGE Strict #-}

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

defaultKeybinds =
  InputState
    { _zoom =
        -- (ScrollState 0),
      (ButtonAxisState (V2 KeycodeB KeycodeO) (V2 False False)),
      _movement =
        DirectionalInput
          -- (ButtonState KeycodeUp False)
          -- (ButtonState KeycodeDown False)
          -- (ButtonState KeycodeLeft False)
          -- (ButtonState KeycodeRight False)
          (ButtonState KeycodeM False)
          (ButtonState KeycodeT False)
          (ButtonState KeycodeS False)
          (ButtonState KeycodeN False),
      -- _quit = ButtonState KeycodeEscape False
      _quit = ButtonState KeycodeQ False
    }

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module FRPEngine.Input.Types where

import GHC.Generics (Generic)
import Control.Lens
import SDL
import Control.DeepSeq

instance NFData SDL.Keycode

-- pr here means press release. Check FRPEngine.Input.Internal.Types for more info.
type PRCount = Int

data Btn = Btn {_key :: !Keycode, _pressed :: !Bool}
  deriving (Show, Generic, NFData)

makeLenses ''Btn

data Input =
    InpBtn !PRCount !Btn
  | InpBtn2D !(V2 Btn)
  | InpBtn4D !(V4 Btn)
  | InpScroll !Int
  | InpCloseWindow !Bool
  deriving (Show, Generic, NFData)

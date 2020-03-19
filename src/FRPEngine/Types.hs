{-# LANGUAGE TemplateHaskell #-}

module FRPEngine.Types where

import FRPEngine.Collision.Types
import Control.Lens
import Linear

data Obj a spriteSelect
  = Obj
      { _pos :: V2 a,
        _size :: V2 a,
        _rot :: a,
        _spr :: spriteSelect,
        _centerRender :: Bool
      }
  deriving (Show)

makeLenses ''Obj

data CollObj a spriteSelect
  = CollObj
      { _coll :: [[Pt' a]],
        _obj :: Obj a spriteSelect
      }
  deriving (Show)

makeLenses ''CollObj

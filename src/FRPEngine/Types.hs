{-# LANGUAGE TemplateHaskell #-}

module FRPEngine.Types where

import FRPEngine.Collision.Types
import Control.Lens
import Linear

data Object a spriteSelect
  = Object
      { _pos :: V2 a,
        _size :: V2 a,
        _rot :: a,
        _spr :: spriteSelect
      }
  deriving (Show)

makeLenses ''Object

data CollObj spriteSelect
  =  CollObj
      { _coll :: [[Pt' Double]],
        _tObj :: Object Double spriteSelect
      }
  deriving (Show)

makeLenses ''CollObj

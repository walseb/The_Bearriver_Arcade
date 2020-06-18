{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module FRPEngine.Types where

import Control.Lens
import Data.Aeson
import FRPEngine.Physics.Collision.Types
import GHC.Generics
import Linear

type Number a = (RealFloat a, Epsilon a, Show a)

data Obj a spriteSelect
  = Obj
      { _pos :: V2 a,
        _vel :: V2 a,
        _rot :: a,
        _size :: V2 a,
        _spr :: spriteSelect,
        _centerRender :: Bool
      }
  deriving (Generic, Show)

data CollObj a spriteSelect
  = CollObj
      { _coll :: [[Pt' a]],
        _obj :: Obj a spriteSelect
      }
  deriving (Generic, Show)

makeLenses ''CollObj

makeLenses ''Obj

-- V2 json instances
instance (FromJSON a) => FromJSON (V2 a)
instance (ToJSON a) => ToJSON (V2 a)

instance (FromJSON a, FromJSON spriteSelect) => FromJSON (Obj a spriteSelect)
instance (FromJSON a, FromJSON spriteSelect) => FromJSON (CollObj a spriteSelect)

instance (ToJSON a, ToJSON spriteSelect) => ToJSON (Obj a spriteSelect)
instance (ToJSON a, ToJSON spriteSelect) => ToJSON (CollObj a spriteSelect)

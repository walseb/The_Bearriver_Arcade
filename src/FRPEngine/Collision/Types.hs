module FRPEngine.Collision.Types where

import Linear (V2 (..))

type Pt' a = V2 a

type Score = Int
data PlayerCollided = HitUnlandable | HitLandable Score | NoHit

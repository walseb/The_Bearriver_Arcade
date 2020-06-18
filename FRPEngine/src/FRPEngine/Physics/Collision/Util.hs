{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRPEngine.Physics.Collision.Util where

import FRPEngine.Physics.Collision.Types
import FRPEngine.Types
import Linear

-- Takes rad as rot
rotateAroundAxis :: (Number a) => a -> V2 a -> V2 a -> V2 a
rotateAroundAxis 0 a _ =
  a
rotateAroundAxis theta (V2 x y) (V2 xO yO) =
  V2 x' y'
  where
    x' = xO + (x - xO) * cos theta - (y - yO) * sin theta
    y' = yO + (x - xO) * sin theta + (y - yO) * cos theta

degToRad :: (Floating a) => a -> a
degToRad theta = theta / 180 * pi

-- Theta is measured in degrees and translated to rad
moveAlongAxis :: (Floating a) => V2 a -> a -> a -> V2 a
moveAlongAxis (V2 x y) dist theta =
  V2 x1 y1
  where
    x1 = x + sin theta * dist
    y1 = y + cos theta * dist

getCollisionPointsPos :: (Number a) => CollObj a b -> [[Pt' a]]
getCollisionPointsPos (CollObj coll (Obj pos _ rot size _ rotCenter)) =
  (fmap . fmap) (\pt -> rotateAroundAxis (- (degToRad rot)) ((pos + (size * pt)) - centerOffset) pos) coll
  where
    centerOffset =
      case rotCenter of
        True -> size / 2
        False -> 0

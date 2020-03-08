module Collision.AABB
  ( collisionAABBCheck,
  )
where

import Linear

collisionAABBCheck :: (Integral a) => (V2 a, V2 a) -> (V2 a, V2 a) -> Bool
collisionAABBCheck
  -- Box1
  (V2 x0 y0, V2 sizeX0 sizeY0)
  -- Box2
  (V2 x1 y1, V2 sizeX1 sizeY1) =
    x0 < x1 + sizeX1
      && x0 + sizeX0 > x1
      && y0 < y1 + sizeY1
      && sizeY0 + y0 > y1

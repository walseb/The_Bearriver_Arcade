{-# LANGUAGE LambdaCase #-}

module FRPEngine.Collision.Internal.GJKLegacy where

import FRPEngine.Collision.Util
import Control.Lens
import FRPEngine.Types
import Linear
import FRPEngine.Collision.Internal.GJK

-- Completely solves collision by moving the player up and re-trying collision test
solveCollision :: Scene -> MovingState -> Player
solveCollision scene (MovingState player enemies) =
  case collidesWrap scene (MovingState player enemies) of
    Just True -> solveCollision scene (MovingState (((pLiving . lObject . pos . _y) `over` (+ 8)) player) enemies)
    Just False -> (Player (Living False (player ^. pLiving . lObject)) (player ^. score))
    Nothing -> player

type PlayerHitLandingspot = Bool

-- If nothing then player didn't hit anything. If Just False then player hit the terrain. If Just True then player hit landing spot
collidesWrap :: Scene -> MovingState -> Maybe PlayerHitLandingspot
collidesWrap (Scene terrain landingSpots) (MovingState player enemies) =
  case playerHitTerrain of
    True ->
      Just False
    False ->
      case playerHitLandingspot of
        True ->
          Just True
        False ->
          Nothing
  where
    playerObj = [objToRect (player ^. (pLiving . lObject))]
    playerHitTerrain =
      collides'
        playerObj
        ([((toPt (enemies ^. (to head . lObject))))] ++ ((^. (coll)) =<< terrain))
    playerHitLandingspot =
      collides'
        playerObj
        ([((toPt (enemies ^. (to head . lObject))))] ++ ((^. (lTerrain . coll)) =<< landingSpots))

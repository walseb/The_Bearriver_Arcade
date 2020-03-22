{-# LANGUAGE LambdaCase #-}

module FRPEngine.Collision.GJK where

import FRPEngine.Collision.GJKInternal.Support
import Control.Applicative
import GJK.Collision
import FRPEngine.Collision.Types
import FRPEngine.Types
import FRPEngine.Collision.Util

-- TODO: Performance: Usually the first arg is the same a lot of this time, does ghc see this and stores the value of the first calculation on that?
collidesObj :: (RealFloat a) => CollObj a b -> CollObj a b -> Bool
collidesObj a b  =
  collides' (getCollisionPointsPos a) (getCollisionPointsPos b)

collides' :: (RealFloat a) => [[Pt' a]] -> [[Pt' a]] -> Bool
collides' pts pts' =
  any
    ( \case
        Just True -> True
        _ -> False
    )
    $ liftA2 collides pts pts'

collides :: (RealFloat a) => [Pt' a] -> [Pt' a] -> Maybe Bool
collides pts pts' = collision 5 (pts, polySupport') (pts', polySupport')

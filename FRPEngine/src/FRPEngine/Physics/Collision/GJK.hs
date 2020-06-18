{-# LANGUAGE LambdaCase #-}

module FRPEngine.Physics.Collision.GJK where

import Data.Maybe
import Control.Applicative
import FRPEngine.Physics.Collision.Types
import FRPEngine.Physics.Collision.Util
import FRPEngine.Types
import FRPEngine.Physics.Collision.GJKInternal.Support
import GJK.Collision

-- TODO: Performance: Usually the first arg is the same a lot of this time, does ghc see this and store the value of the first calculation on that?
collidesObj :: (Number a) => CollObj a b -> CollObj a b -> Bool
collidesObj a b =
  collides' (getCollisionPointsPos a) (getCollisionPointsPos b)

collides' :: (Number a) => [[Pt' a]] -> [[Pt' a]] -> Bool
collides' a b =
  or $ (fromMaybe False) <$> ((liftA2 collides a b) :: [Maybe Bool])

collides :: (Number a) => [Pt' a] -> [Pt' a] -> Maybe Bool
collides pts pts' = collision 5 (pts, polySupport') (pts', polySupport')

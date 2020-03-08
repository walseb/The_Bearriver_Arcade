{-# LANGUAGE Arrows #-}

module FRPEngine.Ship where

import FRPEngine.Collision.Util (moveAlongAxis, degToRad)
import Control.Lens
import FRP.Yampa
import Linear
import FRPEngine.Physics
import FRPEngine.Types
import FRPEngine.YampaUtils.Types ()

thrustForce :: (RealFloat a) => a
thrustForce = 3000

shipMovement :: (RealFloat a) => V2 a -> V2 a -> Double -> SF (Bool, a) (V2 a, V2 a, Double)
shipMovement initPos initVelocity initFuel = proc (thrusterPressed, rot) -> do
  fuel <- integralFrom ((realToFrac initFuel) :: Double) -< (if thrusterPressed then -1 else 0)
  vel <- integralFrom initVelocity -< objectGravity + (if (thrusterPressed && fuel > 0) then moveAlongAxis (V2 0 0) thrustForce (degToRad rot) else 0)
  pos <- integralFrom initPos -< vel
  returnA -< (pos, vel, fuel)

shipControl :: (RealFloat a) => Object a w -> V2 a -> Double -> SF (V2 a) (Object a w, V2 a, a, Double)
shipControl initObj initVel initFuel = proc inputDir -> do
  let movement2 = (inputDir ^. _y) > 0
  rot <- shipRotationSwitch (realToFrac (initObj ^. rot)) -< realToFrac $ inputDir ^. _x
  -- TODO fix this conversion
  (pos, vel, fuel) <- shipMovement (initObj ^. pos) initVel initFuel -< (movement2, realToFrac rot)
  returnA -< ((Object pos (initObj ^. size) (realToFrac rot) (initObj ^. spr)), vel, realToFrac rot, fuel)

shipRotationSwitch :: Double -> SF Double Double
shipRotationSwitch initRot = proc turn -> do
  -- Stop moving or something if a collision is detected
  rot <- integralFrom initRot -< turn * 100
  -- rot <- integralFrom initRot -< rotVel
  returnA -< rot

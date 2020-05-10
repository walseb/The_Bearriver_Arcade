module FRPEngine.Input.Utils where

import FRPEngine.Input.Types
import Linear

vectorizeMovement :: (RealFloat a) => DirectionalInput -> V2 a
vectorizeMovement
  ( DirectionalInput
      (ButtonState _ a0 _)
      (ButtonState _ a1 _)
      (ButtonState _ a2 _)
      (ButtonState _ a3 _)
    ) =
    V2
      -- Horizontal
      (boolToInt a2 - boolToInt a3)
      -- Vertical
      (boolToInt a1 - boolToInt a0)
    where
      boolToInt :: (RealFloat a) => Bool -> a
      boolToInt a = if a then 0 else 1
vectorizeMovement _ = error "Trying to vectorize unsupported input"

accumCount :: (RealFloat a) => a -> KeyState -> a
accumCount s (ButtonAxisState _ (V2 True _)) = s + 1
accumCount s (ButtonAxisState _ (V2 _ True)) = s - 1
accumCount s (ButtonAxisState _ _) = s
accumCount s (ScrollState scrollDist) = s - (realToFrac scrollDist)
accumCount _ a = error ("Error: bound key " ++ show a ++ " not supported for accumulation input role")

accumLimit :: (RealFloat a) => V2 a -> a -> KeyState -> a
accumLimit (V2 limMax limMin) s key = limit $ accumCount s key
  where
    limit a
      | a > limMax = limMax
      | a < limMin = limMin
      | otherwise = a

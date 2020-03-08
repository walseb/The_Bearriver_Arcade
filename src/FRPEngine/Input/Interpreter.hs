module FRPEngine.Input.Interpreter where

import FRPEngine.Input.Types
import Linear

vectorizeMovement :: (RealFloat a) => DirectionalInput -> V2 a
vectorizeMovement
  ( DirectionalInput
      (ButtonState _ a0)
      (ButtonState _ a1)
      (ButtonState _ a2)
      (ButtonState _ a3)
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

accumCount :: Int -> KeyState -> Int
accumCount s (ButtonAxisState _ (V2 True _)) = s + 1
accumCount s (ButtonAxisState _ (V2 _ True)) = s -1
accumCount s (ButtonAxisState _ _) = s 
accumCount s (ScrollState scrollDist) = s + scrollDist

accumLimit :: V2 Int -> Int -> KeyState -> Int
accumLimit (V2 limMax limMin) s key = limit $ accumCount s key
  where
    limit a
      | a > limMax = limMax
      | a < limMin = limMin
      | otherwise = a

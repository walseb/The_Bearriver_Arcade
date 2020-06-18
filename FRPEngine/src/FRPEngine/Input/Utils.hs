module FRPEngine.Input.Utils where

import FRPEngine.Input.Types
import FRPEngine.Types
import Linear

vectorizeMovement :: (Number a) => Input -> V2 a
vectorizeMovement
  ( InpBtn4D
      ( V4
          (Btn _ a0)
          (Btn _ a1)
          (Btn _ a2)
          (Btn _ a3)
        )
    ) =
    V2
      -- Horizontal
      (boolToInt a2 - boolToInt a3)
      -- Vertical
      (boolToInt a1 - boolToInt a0)
    where
      boolToInt :: (Number a) => Bool -> a
      boolToInt a = if a then 0 else 1
vectorizeMovement _ = error "Trying to vectorize unsupported input"

accumCount :: (Number a) => a -> Input -> a
accumCount s (InpBtn2D (V2 (Btn _ True) _)) = s + 1
accumCount s (InpBtn2D (V2 _ (Btn _ True))) = s - 1
accumCount s (InpBtn2D _) = s
accumCount s (InpScroll scrollDist) = s - realToFrac scrollDist
accumCount _ a = error ("Error: bound key " ++ show a ++ " not supported for accumulation input role")

accumLimit :: (Number a) => V2 a -> a -> Input -> a
accumLimit (V2 limMax limMin) s key = limit $ accumCount s key
  where
    limit a
      | a > limMax = limMax
      | a < limMin = limMin
      | otherwise = a

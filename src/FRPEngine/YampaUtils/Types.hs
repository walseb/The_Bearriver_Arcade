{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FRPEngine.YampaUtils.Types
  (
  )
where

import FRP.Yampa
import Linear

instance (Eq a, Floating a) => VectorSpace (V2 a) a where
  zeroVector = V2 0 0
  a *^ (V2 x y) = V2 (a * x) (a * y)
  (V2 x y) ^/ a = V2 (x / a) (y / a)
  negateVector (V2 x y) = V2 (- x) (- y)
  (V2 x1 y1) ^+^ (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
  (V2 x1 y1) ^-^ (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
  (V2 x1 y1) `dot` (V2 x2 y2) = x1 * x2 + y1 * y2

instance (Eq a, Floating a) => VectorSpace (V1 a) a where
  zeroVector = 0
  a *^ (V1 x) = V1 (a * x)
  (V1 x) ^/ a = V1 (x / a)
  negateVector x = (- x)
  (V1 x1) ^+^ (V1 x2) = V1 (x1 + x2)
  (V1 x1) ^-^ (V1 x2) = V1 (x1 - x2)
  (V1 x1) `dot` (V1 x2) = x1 * x2

-- instance VectorSpace CDouble CDouble where
--     zeroVector = 0
--     a *^ x = a * x
--     x ^/ a = x / a
--     negateVector x = (-x)
--     x1 ^+^ x2 = x1 + x2
--     x1 ^-^ x2 = x1 - x2
--     x1 `dot` x2 = x1 * x2

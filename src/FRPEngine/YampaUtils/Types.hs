{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FRPEngine.YampaUtils.Types
  (
  )
where

import FRP.Yampa
import Linear as L

instance (Eq a, Floating a) => VectorSpace (V2 a) a where
  zeroVector = L.zero
  (*^) = (L.*^)
  (^/) = (L.^/)
  negateVector = L.negated
  (^+^) = (L.^+^)
  (^-^) = (L.^-^)
  dot = L.dot

instance (Eq a, Floating a) => VectorSpace (V1 a) a where
  zeroVector = L.zero
  (*^) = (L.*^)
  (^/) = (L.^/)
  negateVector = L.negated
  (^+^) = (L.^+^)
  (^-^) = (L.^-^)
  dot = L.dot

-- -- instance VectorSpace CDouble CDouble where
--     zeroVector = 0
--     a *^ x = a * x
--     x ^/ a = x / a
--     negateVector x = (-x)
--     x1 ^+^ x2 = x1 + x2
--     x1 ^-^ x2 = x1 - x2
--     x1 `dot` x2 = x1 * x2

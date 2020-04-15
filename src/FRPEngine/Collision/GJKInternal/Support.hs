module FRPEngine.Collision.GJKInternal.Support where

import Data.Maybe (fromMaybe)
import FRPEngine.Collision.Types
import GJK.Collision
import GJK.Point (Pt)
import Linear (V2 (..), dot)

-- TODO: As you see here I'm redefining this to use V2 but GJK really wants stuff to be in the format (Double, Double) so It's kinda awkward
polySupport' :: (RealFloat a) => [Pt' a] -> Pt -> Maybe Pt
polySupport' list (dX, dY) =
  let dotList = fmap (dot (V2 dX dY)) ((fmap . fmap) realToFrac list)
      decorated = zip dotList list
      maybemax = safeMaximum decorated
   in case maybemax of
        Just (_, V2 pX pY) -> Just (realToFrac pX, realToFrac pY)
        _ -> Nothing

-- Comes from the GJK package, it doesn't export it so I have to redefine it
safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum list = Just $ maximum list

debugIsCollision :: (RealFloat a) => [Pt' a] -> [Pt' a] -> Bool
debugIsCollision a b =
  fromMaybe False $ collision 1 (a, polySupport') (b, polySupport')

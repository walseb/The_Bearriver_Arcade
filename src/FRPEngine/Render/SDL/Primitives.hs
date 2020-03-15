{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module FRPEngine.Render.SDL.Primitives (renderEx, renderLine, renderObj, renderText) where

import Control.Monad.IO.Class
import Foreign.C.Types
import SDL.Vect
import SDL.Video.Renderer
import Data.Vector.Storable

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Text (pack)
import qualified SDL as S
import qualified SDL.Font as F
import FRPEngine.Types

-- Render the sprite at the given coordinates.
-- Here spr is the sprite to be rendered
-- pos is the position of the sprite
-- theta is the rotation of sprite
-- center is the center to rotate around
-- Flip is weather or not to flip the sprite on x and y axis
-- sourceRect is the source rectangle to copy, or 'Nothing' for the whole texture
-- destRect is the destination rectangle to copy to, or 'Nothing' for the whole rendering target. The texture will be stretched to fill the given rectangle.
copyEx' :: MonadIO m => Renderer -> Texture -> V2 CInt -> Maybe (Rectangle CInt) -> V2 CInt -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
copyEx' rend spr pos sourceRect destRect =
  copyEx
    rend
    spr
    sourceRect
    (Just (Rectangle (P pos) destRect))
{-# INLINE copyEx' #-}

-- Render ex except with distortions based on zoom level and whatever
-- deltaPos is the distance between the player and the camera. This means the distance everything needs to be moved by to put the camera at the correct position
renderEx :: MonadIO m => Renderer -> V2 CInt -> V2 CInt -> Texture -> V2 CInt -> Maybe (Rectangle CInt) -> V2 CInt -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
renderEx rend deltaPos zoomLevel spr pos sourceRect destRect theta center =
  copyEx'
    rend
    spr
    ((pos + deltaPos) `v2Div` zoomLevel)
    sourceRect
    (destRect `v2Div` zoomLevel)
    theta
    center'
    where
      v2Div = liftA2 div
      center' = case center of
                  Just (P c) -> Just $ P $ c `v2Div` zoomLevel
                  Nothing -> Nothing
{-# INLINE renderEx #-}

renderLine :: (MonadIO m) => Renderer -> V2 CInt -> V2 CInt-> [[V2 CInt]] -> m ()
renderLine rend deltaPos zoomLevel points =
  drawLines rend points'
    where
      v2Div = liftA2 div
      points' = fromList $
            P <$>
            (fmap (+ deltaPos `v2Div` zoomLevel) (join points))
{-# INLINE renderLine #-}

renderObj :: (RealFrac a, Integral b) => V2 a -> (Object a w -> S.Texture) -> b -> S.Renderer -> Bool -> Object a w -> IO ()
renderObj deltaPos res zoomLevel getSprite renderFromCenter obj =
  renderSpr
    getSprite
    (res obj)
    (obj ^. pos)
    (obj ^. size)
    (realToFrac (obj ^. rot))
    renderFromCenter
  where
    renderEx' =
      ( \rend rotCenter spr pos size theta ->
          renderEx
            rend
            (fmap floor deltaPos)
            (fmap fromIntegral (pure zoomLevel))
            spr
            (fmap floor (negateYAxis pos))
            Nothing
            (fmap floor size)
            theta
            rotCenter
            (V2 False False)
      )
    renderSpr = \rend spr pos size theta renderFromCenter ->
      renderEx'
        rend
        -- Always rotate around center
        Nothing
        spr
        (if renderFromCenter then pos - (negateYAxis (size / 2)) else pos)
        size
        theta
    negateYAxis :: (Num a) => V2 a -> V2 a
    negateYAxis = _y `over` negate
{-# INLINE renderObj #-}

renderText renderer font text pos = do
  fontSurface <- F.solid font (V4 255 255 255 255) (pack text)
  fontTex <- S.createTextureFromSurface renderer fontSurface

  -- Destroy the surface after the texture is created
  S.freeSurface fontSurface

  S.copy renderer fontTex Nothing (Just pos)

  -- Destroy the texture after it's rendered
  S.destroyTexture fontTex
{-# INLINE renderText #-}

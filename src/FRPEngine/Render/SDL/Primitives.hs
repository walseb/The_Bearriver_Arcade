module FRPEngine.Render.SDL.Primitives
  ( renderLine,
    renderObj,
    renderText,
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (pack)
import Data.Vector.Storable
import FRPEngine.Types
import Foreign.C.Types
import qualified SDL as S
import qualified SDL.Font as F
import SDL.Vect
import SDL.Video.Renderer

screenSize :: (Num a) => V2 a
screenSize = V2 1280 720
-- screenSize = V2 2560 1440
-- screenSize = V2 1920 1080

copyEx' :: MonadIO m => Renderer -> Texture -> Maybe (Rectangle CInt) -> V2 CInt -> V2 CInt -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
copyEx' rend spr sourceRect pos destRect =
  copyEx
    rend
    spr
    sourceRect
    (Just (Rectangle (P pos) destRect))
{-# INLINE copyEx' #-}

renderEx' :: (RealFrac a, MonadIO m) => Renderer -> Maybe (V2 a) -> Texture -> V2 a -> V2 a -> a -> Bool -> a -> V2 a -> m ()
renderEx' rend rotCenter spr pos size' theta renderFromCenter zoom playerPos =
  copyEx'
    rend
    spr
    Nothing
    -- Pos of object
    (fmap floor ((pos' + (screenMiddle - negateYAxis playerPos)) / pure zoom))
    (fmap floor (size' / pure zoom))
    (realToFrac theta)
    (rotCenter >>= (\center -> Just $ P $ fmap floor $ center / pure zoom))
    (V2 False False)
  where
    -- Set pos depending on if rendering should be done from center or not
    pos' =
      negateYAxis
        ( case renderFromCenter of
            True -> pos - negateYAxis (size' / 2)
            False -> pos
        )
    screenMiddle = (screenSize / 2) * pure zoom
    negateYAxis :: (Num a) => V2 a -> V2 a
    negateYAxis = _y `over` negate
{-# INLINE renderEx' #-}

-- Objects that don't render from center instead render from bottom left
renderObj :: (RealFrac a) => V2 a -> (Object a w -> S.Texture) -> a -> S.Renderer -> Bool -> Object a w -> IO ()
renderObj playerPos res zoomLevel renderer renderFromCenter obj =
  renderEx'
    renderer
    Nothing
    (res obj)
    (obj ^. pos)
    (obj ^. size)
    (obj ^. rot)
    renderFromCenter
    zoomLevel
    playerPos
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

renderLine :: (MonadIO m) => Renderer -> V2 CInt -> V2 CInt -> [[V2 CInt]] -> m ()
renderLine rend playerPos zoomLevel points =
  drawLines rend points'
  where
    v2Div = liftA2 div
    points' =
      fromList $
        P
          <$> (fmap (+ playerPos `v2Div` zoomLevel) (join points))
{-# INLINE renderLine #-}

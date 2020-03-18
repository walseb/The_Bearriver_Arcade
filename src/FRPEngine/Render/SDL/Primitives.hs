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
-- screenSize = V2 1280 720
-- screenSize = V2 2560 1440
screenSize = V2 1920 1080

copyEx' :: MonadIO m => Renderer -> Texture -> Maybe (Rectangle CInt) -> V2 CInt -> V2 CInt -> CDouble -> Maybe (Point V2 CInt) -> V2 Bool -> m ()
copyEx' rend spr sourceRect pos destRect =
  copyEx
    rend
    spr
    sourceRect
    (Just (Rectangle (P pos) destRect))
{-# INLINE copyEx' #-}

renderEx' :: (RealFrac a, MonadIO m) => Renderer -> Maybe (V2 a) -> Texture -> V2 a -> V2 a -> a -> Bool -> a -> V2 a -> m ()
renderEx' rend rotCenter spr _pos size theta renderFromCenter zoom _camPos =
  copyEx'
    rend
    spr
    Nothing
    -- Pos of object
    (fmap floor ((pos + (screenMiddle - camPos)) / pure zoom))
    (fmap floor (size / pure zoom))
    (realToFrac theta)
    (rotCenter >>= (\center -> Just $ P $ fmap floor $ center / pure zoom))
    (V2 False False)
  where
    camPos = negateYAxis _camPos
    -- SDL renders from top left. This offsets all game state so that it acts as if bottom left rendering was used
    pos =
      negateYAxis
        ( _pos
            +
            -- Some objects you want to render from center
            ( case renderFromCenter of
                -- Objects that render from center needs to be moved half its size up and half its size right
                True -> negateXAxis size / 2
                -- Objects that render from bottom left needs to move down its size
                False -> V2 0 (size ^. _y)
            )
        )
    screenMiddle = (screenSize / 2) * pure zoom
    negateYAxis :: (Num a) => V2 a -> V2 a
    negateYAxis = _y `over` negate
    negateXAxis :: (Num a) => V2 a -> V2 a
    negateXAxis = _x `over` negate
{-# INLINE renderEx' #-}

-- Objects that don't render from center instead render from bottom left
renderObj :: (RealFrac a) => V2 a -> (Object a w -> S.Texture) -> a -> S.Renderer -> Bool -> Object a w -> IO ()
renderObj camPos res zoomLevel renderer renderFromCenter obj =
  renderEx'
    renderer
    Nothing
    (res obj)
    (obj ^. pos)
    (obj ^. size)
    (obj ^. rot)
    renderFromCenter
    zoomLevel
    camPos
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
renderLine rend camPos zoomLevel points =
  drawLines rend points'
  where
    v2Div = liftA2 div
    points' =
      fromList $
        P
          <$> (fmap (+ camPos `v2Div` zoomLevel) (join points))
{-# INLINE renderLine #-}

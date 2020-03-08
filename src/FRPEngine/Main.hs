module Main where

import Control.Concurrent
  ( newMVar,
    swapMVar,
  )
import Data.String (fromString)
import FRP.Yampa
import Linear
import qualified SDL as S

initSDL :: IO (S.Renderer, S.Window)
initSDL = do
  S.initializeAll
  window <- S.createWindow (fromString "My SDL Application") (S.WindowConfig True False False S.Maximized S.NoGraphicsContext S.Wherever False (V2 800 600) True)
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  return (renderer, window)

runSDL loadResources run = do
  (renderer, window) <- initSDL
  senseInput <- getSenseInput

  resources <- loadResources renderer
  _ <- run renderer senseInput resources

  S.destroyRenderer renderer
  S.destroyWindow window
  where
    getSenseInput = do
      lastInteraction <- newMVar =<< S.time
      let senseInput _canBlock = do
            currentTime <- S.time
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            events <- Event <$> S.pollEvents
            return (dt, Just events)
      pure senseInput

-- main =
--   runSDL
--     (\rend -> getResources rend)
--     (\renderer senseInput resources -> reactimate (return NoEvent) senseInput (\_ -> render renderer resources) (update initialGame))

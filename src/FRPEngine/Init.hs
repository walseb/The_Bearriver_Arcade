module FRPEngine.Init where

import Control.Concurrent
  ( newMVar,
    swapMVar,
  )
import Data.String (fromString)
import FRP.Yampa
import Linear
import qualified SDL as S

initSDL :: String -> IO (S.Renderer, S.Window)
initSDL windowName = do
  S.initializeAll
  window <- S.createWindow (fromString windowName) (S.WindowConfig True False False S.Maximized S.NoGraphicsContext S.Wherever False (V2 800 600) True)
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  return (renderer, window)

runSDL windowName loadResources run = do
  (renderer, window) <- initSDL windowName
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

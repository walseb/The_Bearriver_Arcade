module FRPEngine.Init where

import Control.Concurrent
  ( newMVar,
    readMVar,
    swapMVar,
    threadDelay,
  )
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)
import qualified FRP.Yampa as Yampa
import Linear
import qualified SDL as S
import System.Directory (doesFileExist)

frameCap :: Double
frameCap = 1 / 200

-- Longest time can jump forward when PC is slow. This is so that things don't happen too fast when frames are low
-- So if the PC takes longer between frames than a 60 fps frame time here, it will instead just jump forward at 60 fps speed
frameMaxJump :: Double
frameMaxJump = 1 / 60

initSDL :: String -> S.WindowMode -> IO (S.Renderer, S.Window)
initSDL windowName windowMode = do
  S.initializeAll
  window <- S.createWindow (fromString windowName) (S.WindowConfig True False False windowMode S.NoGraphicsContext S.Wherever True (V2 800 600) True)
  renderer <- S.createRenderer window (-1) S.defaultRenderer
  pure (renderer, window)

gameStateFile = "SaveGame"

runSDL :: (FromJSON s, ToJSON s) => Bool -> S.WindowMode -> String -> (S.Renderer -> IO t) -> (Maybe s -> S.Renderer -> (p -> IO (Double, Maybe (Yampa.Event [S.Event]))) -> t -> IO s) -> IO ()
runSDL debug windowMode windowName loadResources run = do
  (renderer, window) <- initSDL windowName windowMode
  resources <- loadResources renderer
  sense <- getSense

  fileExist <- doesFileExist gameStateFile

  loadedGameState <-
    if fileExist
      then eitherDecodeStrict <$> B.readFile gameStateFile
      else pure (Left "No game state file")

  let loadedGameState' =
        case loadedGameState of
          Left err -> seq (Prelude.putStrLn ("Game state load error: " ++ err)) Nothing
          Right res -> Just res

  -- Run FRP network
  savedGameState <- run loadedGameState' renderer sense resources

  -- Save game state
  BL.writeFile gameStateFile (encode savedGameState)

  S.destroyRenderer renderer
  S.destroyWindow window
  where
    -- Delta time
    getDeltaTime :: IO (IO Double, IO Double)
    getDeltaTime = do
      lastTime <- newMVar =<< S.time
      let setDt = do
            -- Get time
            newTime <- S.time
            -- Return the delta time
            -- swapMVar returns the last value, not the new value
            (newTime -) <$> swapMVar lastTime newTime
      let readDt = do
            newTime <- S.time
            (newTime -) <$> readMVar lastTime
      pure (setDt, readDt)
    -- Events
    getEvents = do
      events <- S.pollEvents
      pure $ Yampa.Event events
    -- Debug
    oneSecondTimer = do
      delta <- newMVar 0
      let updateTimer dt = do
            deltaVal <- readMVar delta
            swapMVar delta (if deltaVal + dt > 1 then 0 else deltaVal + dt)
      pure updateTimer
    profileFrameTime dt = print ("Framerate: " ++ show (1 / dt))
    getSense = do
      -- Init get delta time
      (setDt, readDt) <- getDeltaTime
      getTimer <- oneSecondTimer
      let sense _canBlock = do
            -- Get the delta time without updating it
            dt' <- readDt
            case dt' < frameCap of
              True -> threadDelay (floor ((frameCap - dt') * 10 ^ 6))
              False -> pure ()
            -- Get the delta time and set it
            dt <- setDt
            -- Get events
            events <- getEvents
            -- Debug frame time
            case debug of
              True -> do
                test <- getTimer dt
                case test == 0 of
                  True -> profileFrameTime dt
                  False -> pure ()
                pure ()
              False -> pure ()
            pure
              -- Prevent delta time from getting higher than frametime for 60 fps
              (min dt frameMaxJump, Just events)
      pure sense

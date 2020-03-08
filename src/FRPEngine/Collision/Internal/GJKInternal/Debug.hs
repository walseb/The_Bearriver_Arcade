module FRPEngine.Collision.Internal.GJKInternal.Debug where

import FRPEngine.Collision.Types
import FRPEngine.Collision.Util
import FRPEngine.Collision.Internal.GJKInternal.Support
import Control.Lens
import Foreign.C.Types
import Linear
import FRPEngine.Types
import Control.Monad
import qualified Debug.Trace as Tr

debugRenderHitbox :: (RealFloat a) => (V2 a -> IO ()) -> Object a -> IO [()]
debugRenderHitbox renderFunc obj =
  sequence $ fmap renderFunc (objToRect (obj ^. pos) (obj ^. size) (obj ^. rot))

  -- let test = (toPt ((obj ^. pos) + ((obj ^. size) / 2)) (obj ^. size) (obj ^. rot))
  --     test2 = fmap (\a -> fmap realToFrac a) test
  --  in mapM renderFunc test2

  -- mapM (\terr' -> (fmap (\coll -> debugRenderThing renderFunc sprite  0) ((fmap . fmap) floor (concat (terr' ^. coll))))) terr
   -- let test2 = fmap (\a -> fmap floor a) pts
   -- in mapM (debugRenderThing renderFunc sprite (V2 100 100) 0) test2


debugHitboxes :: (V2 Double -> IO ()) -> [Living] -> [Terrain] -> IO [()]
debugHitboxes renderFunc living terr =
  do
    _ <- sequence $ (fmap (debugRenderHitbox renderFunc) (fmap (^. lObject) living))

    -- Tr.traceM ("Terrs points" ++ (join (join (fmap (\a -> fmap show (a ^. coll)) terrs))))
    -- Tr.traceM ("Terrs points total: " ++ (show bunchOfPts))
    -- Tr.traceM ("I got TEST: " ++ (show terr))
    -- Tr.traceM ("Terrs points" ++ (show bunchOfPts))
    -- Tr.traceM ("Terrs points'" ++ (show bunchOfPts'))

    sequence $ (join . join) $ ((fmap . fmap . fmap) renderFunc bunchOfPts)
    where bunchOfPts = fmap (^. coll) terr

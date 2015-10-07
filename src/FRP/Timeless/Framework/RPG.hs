module FRP.Timeless.Framework.RPG where

import FRP.Timeless
import qualified SDL as SDL

sTestOutBox :: SDL.Renderer -> Signal s IO () ()
sTestOutBox w = mkKleisli_ $ box w
  where
    box :: SDL.Renderer -> () -> IO ()
    box w _ = do
      SDL.clear w
      SDL.present w
      return ()

gameBox :: SDL.Renderer -> Signal s IO () ()
gameBox w = proc _ -> do
  sTestOutBox w -< ()

gameSession = clockSession_

initApp :: IO (Signal s IO () ())
initApp = do
  SDL.initialize [SDL.InitEverything]
  window <- SDL.createWindow "RPG Framework" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  return $ gameBox renderer

runApp = do
  box <- initApp
  runBox gameSession box

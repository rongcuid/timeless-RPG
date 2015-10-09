-- |
-- Module:     FRP.Timeless.Framesork.RPG
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Framework.RPG where

import Prelude hiding ((.), id)
import FRP.Timeless
import qualified SDL as SDL
import qualified Data.Tiled as Tiled
import FRP.Timeless.Framework.RPG.Render

-- * Tests
sTestOutBox :: SDL.Window -> SDL.Renderer -> Signal s IO () ()
sTestOutBox w rDest = proc _ -> do
  mkKleisli_ $ box -< ()
  
  where
    box :: () -> IO ()
    box _ = do
      SDL.clear rDest
      SDL.present rDest
      return ()

sTestMap = sLoadMap "examples/desert.tmx"

--sTestRenderMap w = sRenderMap w "examples/desert.tmx"
      
-- * App descriptions
sLoadMap :: FilePath -> Signal s IO () Tiled.TiledMap
sLoadMap file = mkConstM_ $ Tiled.loadMapFile file

gameBox :: SDL.Window -> SDL.Renderer -> Signal s IO () ()
gameBox w rDest = proc _ -> do
  sTestOutBox w rDest -< ()

gameSession = clockSession_

initApp :: IO (Signal s IO () ())
initApp = do
  SDL.initialize [SDL.InitEverything]
  window <- SDL.createWindow "RPG Framework" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  return $ gameBox window renderer

runApp = do
  box <- initApp
  runBox gameSession box

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
import FRP.Timeless.Framework.RPG.Render.Types
import FRP.Timeless.Framework.RPG.Scene.MapScene

import Data.StateVar (($=))

import Debug.Trace as Debug

-- * Tests
sTestOutBox :: SDL.Window -> SDL.Renderer -> Signal s IO [RenderLayer] ()
sTestOutBox w rDest = proc rls -> do
  mkKleisli_ $ box -< rls
  where
    box :: [RenderLayer] -> IO ()
    box rls = do
      SDL.clear rDest
      runRenderLayerStack rDest Nothing rls
      SDL.present rDest
      return ()

testMapLayerStack :: SDL.Window -> SDL.Renderer -> IO [RenderLayer]
testMapLayerStack win ren = mapRenderLayerStack win ren "desert.tmx"

--sTestRenderMap w = sRenderMap w "examples/desert.tmx"
      
-- * App descriptions

testGameBox :: SDL.Window -> SDL.Renderer -> Signal s IO () ()
testGameBox win ren = proc _ -> do
  rls <- snapOnce <<< mkConstM (testMapLayerStack win ren) <<< inhibitsAfter 1 -< ()
  sTestOutBox win ren -< rls

gameSession = clockSession_

initApp :: IO (Signal s IO () ())
initApp = do
  SDL.initialize [SDL.InitEverything]
  window <- SDL.createWindow "RPG Framework" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  --rls <- testMapLayerStack window renderer
  return $ testGameBox window renderer --rls

runApp = do
  box <- initApp
  runBox gameSession box

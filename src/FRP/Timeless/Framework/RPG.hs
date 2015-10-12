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

import Foreign.C.Types (CInt)

import Debug.Trace as Debug

-- * Tests
sTestOutBox :: SDL.Renderer -> Maybe SDL.Texture -> Signal s IO [RenderLayer] ()
sTestOutBox ren tDest = proc rls -> do
  mkKleisli_ $ box -< rls
  where
    box :: [RenderLayer] -> IO ()
    box rls = do
      SDL.clear ren
      runRenderLayerStack ren tDest rls
      SDL.present ren
      return ()

testMapLayerStack :: SDL.Renderer -> IO [RenderLayer]
testMapLayerStack ren = mapRenderLayerStack ren "desert.tmx"

testCamera :: SDL.Renderer
           -> Signal s IO RenderLayer (Signal s IO (Camera CInt) ())
testCamera ren = proc rl -> do
  returnA -< mkKleisli_ $ f rl
  where
    f :: RenderLayer -> Camera CInt -> IO ()
    f rl cam = do
      let srcTex = texture rl
      SDL.clear ren
      SDL.copy ren srcTex cam Nothing
      SDL.present ren
      return ()

-- * App descriptions

testGameBox :: SDL.Renderer -> Maybe SDL.Texture -> Signal s IO () ()
testGameBox ren tex = proc _ -> do
  rls <- runAndHold $ mkConstM (testMapLayerStack ren) -< ()
  sTestOutBox ren tex -< rls

gameSession = clockSession_

initApp :: IO (Signal s IO () ())
initApp = do
  SDL.initialize [SDL.InitEverything]
  window <- SDL.createWindow "RPG Framework" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  return $ testGameBox renderer Nothing --rls

runApp = do
  box <- initApp
  runBox gameSession box

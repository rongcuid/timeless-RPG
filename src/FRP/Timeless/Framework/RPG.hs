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
import Linear
import Linear.Affine
import FRP.Timeless.Framework.RPG.Render
import FRP.Timeless.Framework.RPG.Render.Types
import FRP.Timeless.Framework.RPG.Scene.MapScene

import Data.StateVar (($=))

import Debug.Trace as Debug

-- * Tests
sTestOutBox :: SDL.Renderer -> Signal s IO Scene ()
sTestOutBox ren = proc rls -> do
  mkKleisli_ $ box -< rls
  where
    box :: Scene -> IO ()
    box s = do
      SDL.clear ren
      viewScene ren s
      SDL.present ren
      return ()

testMapScene :: SDL.Renderer -> IO Scene
testMapScene ren = loadMapScene ren "desert.tmx"
                   (Just $ cIntRect (P $ V2 0 0) (V2 800 600))

-- * App descriptions

testGameBox :: SDL.Renderer -> Signal s IO () ()
testGameBox ren = proc _ -> do
  rls <- runAndHold $ mkConstM (testMapScene ren) -< ()
  sTestOutBox ren -< rls

gameSession = clockSession_

initApp :: IO (Signal s IO () ())
initApp = do
  SDL.initialize [SDL.InitEverything]
  window <- SDL.createWindow "RPG Framework" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  return $ testGameBox renderer

runApp = do
  box <- initApp
  runBox gameSession box

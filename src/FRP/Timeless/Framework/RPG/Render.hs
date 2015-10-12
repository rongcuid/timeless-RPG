-- |
-- Module:     FRP.Timeless.Framesork.RPG.Render
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Framework.RPG.Render
       (
         -- * Render Action
         runRenderLayer
       , runRenderLayerStack
       , viewScene
       )
       where

import Prelude hiding ((.), id)
import qualified SDL as SDL
import Linear
import Linear.Affine
import GHC.Word
import Data.StateVar (get, ($=))

import FRP.Timeless
import FRP.Timeless.Framework.RPG.Render.TileLayer
import FRP.Timeless.Framework.RPG.Render.Types


-- | An IO action to run a `RenderLayer`
runRenderLayer :: SDL.Renderer
               -- ^ Target Renderer
               -> Maybe SDL.Texture
               -- ^ Render target, probably a Texture
               -> Camera
               -> Projector
               -> RenderLayer
               -> IO ()
runRenderLayer ren rt' cam prj rl = do
  let srcTex = texture rl
  renderProtectTarget ren rt' srcTex cam prj

-- | Renders a stack of layers, in order
runRenderLayerStack :: SDL.Renderer
                    -- ^ Target Renderer
                    -> Maybe SDL.Texture
                    -- ^ Render target, probably a Texture
                    -> Camera
                    -> Projector
                    -> [RenderLayer]
                    -- ^ Stack of `RenderLayer`s
                    -> IO ()
runRenderLayerStack ren rt' cam prj rls =
  runRenderLayer ren rt' cam prj `mapM_` rls


viewScene :: SDL.Renderer
          -> Scene
          -> IO ()
viewScene ren s = 
  let rls = sceneLayers s
      cam = sceneCamera s
  in 
  runRenderLayerStack ren Nothing cam Nothing rls
      

renderProtectTarget ren rt' srcTex cam prj = do
  -- v Save the previous render target
  rt <- get $ SDL.rendererRenderTarget ren
  -- v Set render target
  SDL.rendererRenderTarget ren $= rt'
  -- v Render
  SDL.copy ren srcTex cam prj
  -- v Restore render target
  SDL.rendererRenderTarget ren $= rt
  

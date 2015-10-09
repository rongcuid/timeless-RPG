-- |
-- Module:     FRP.Timeless.Framesork.RPG.Render
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Framework.RPG.Render where

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
runRenderLayer :: (RenderLayer r) =>
                  SDL.Renderer
               -- ^ Target Renderer
               -> Maybe SDL.Texture
               -- ^ Render target, probably a Texture
               -> r
               -- ^ RenderLayer
               -> IO ()
runRenderLayer ren rt' rl = do
  let srcTex = texture rl

  -- | Save the previous render target
  rt <- get $ SDL.rendererRenderTarget ren
  -- | Set render target
  SDL.rendererRenderTarget ren $= rt'
  -- | Render
  SDL.copy ren srcTex Nothing Nothing
  -- | Restore render target
  SDL.rendererRenderTarget ren $= rt

-- | Renders a stack of layers, in order
runRenderLayerStack :: (RenderLayer r) =>
                       SDL.Renderer
                    -- ^ Target Renderer
                    -> Maybe SDL.Texture
                    -- ^ Render target, probably a Texture
                    -> [r]
                    -- ^ Stack of `RenderLayer`s
                    -> IO ()
runRenderLayerStack ren rt' rls = runRenderLayer ren rt' `mapM_` rls

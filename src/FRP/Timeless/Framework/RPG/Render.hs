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

import FRP.Timeless
import FRP.Timeless.Framework.RPG.Render.TileLayer

renderLayers :: SDL.Renderer
             -> [SDL.Texture]
             -> IO ()
renderLayers renDest txs =
    mapM_ (\tx -> SDL.copy renDest tx Nothing Nothing) txs

sRenderMap :: SDL.Window
           -> FilePath
           -> Signal s IO () ()
sRenderMap win path = proc _ -> do
  rd <- mkConstM_ $ loadTileRenderData win path -< ()
  tx0 <- sLayerRenderer win 0 -< rd
  returnA -< ()

-- |
-- Module:     FRP.Timeless.Framework.RPG.Scene.MapScene
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Framework.RPG.Scene.MapScene
       where

import Prelude hiding ((.), id)
import qualified SDL as SDL
import Control.Monad

import FRP.Timeless
import FRP.Timeless.Framework.RPG.Render
import FRP.Timeless.Framework.RPG.Render.TileLayer
import FRP.Timeless.Framework.RPG.Render.Types

mapRenderLayerStack :: SDL.Window
                    -> SDL.Renderer
                    -> FilePath
                    -> IO [RenderLayer]
mapRenderLayerStack win ren path = do
  trd <- loadTileRenderData ren path

  lay0 <- RenderLayer `liftM` createLayerRenderer win ren 0 trd
  lay1 <- RenderLayer `liftM` createLayerRenderer win ren 1 trd

  return [lay0, lay1]


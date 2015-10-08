-- |
-- Module:     FRP.Timeless.Framesork.RPG.Render
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Framework.RPG.Render where

import Prelude hiding ((.), id)
import FRP.Timeless
import qualified SDL as SDL
import Data.Tiled 

renderMap :: SDL.Renderer -> TiledMap -> IO ()
renderMap r m = do
  return ()

renderLayer :: SDL.Renderer -> Layer -> IO ()
renderLayer = undefined

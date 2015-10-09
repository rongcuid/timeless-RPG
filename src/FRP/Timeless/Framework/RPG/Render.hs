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
import Data.StateVar (get)

import FRP.Timeless
import FRP.Timeless.Framework.RPG.Render.TileLayer

renderLayers :: SDL.Renderer
             -> [SDL.Texture]
             -> IO SDL.Texture
renderLayers renDest txs = do
  mapM_ (\tx -> SDL.copy renDest tx Nothing Nothing) txs
  mtx <- get $ SDL.rendererRenderTarget renDest
  case mtx of
    Just tx -> return tx
    Nothing -> error "[BUG]: Somehow renDest does not have render target"

-- sRenderMap :: SDL.Window
--            -> FilePath
--            -> Signal s IO () SDL.Texture
-- sRenderMap win path =

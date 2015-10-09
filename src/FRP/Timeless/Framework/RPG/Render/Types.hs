-- |
-- Module:     FRP.Timeless.Framework.RPG.Render.Types
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>


module FRP.Timeless.Framework.RPG.Render.Types
       where

import qualified SDL as SDL

class RenderLayer r where
  renderer :: r -> SDL.Renderer
  texture :: r -> SDL.Texture

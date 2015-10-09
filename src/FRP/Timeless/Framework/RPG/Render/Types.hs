-- |
-- Module:     FRP.Timeless.Framework.RPG.Render.Types
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>


module FRP.Timeless.Framework.RPG.Render.Types
       where

import qualified SDL as SDL

class RenderLayerClass r where
  texture :: r -> SDL.Texture

data RenderLayer = forall r . RenderLayerClass r => RenderLayer r
instance RenderLayerClass RenderLayer where
  texture (RenderLayer l) = texture l

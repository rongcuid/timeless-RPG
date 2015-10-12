-- |
-- Module:     FRP.Timeless.Framework.RPG.Render.Types
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>


module FRP.Timeless.Framework.RPG.Render.Types
       where

import qualified SDL as SDL
import Foreign.C.Types (CInt)

class RenderLayerClass r where
  texture :: r -> SDL.Texture

-- | A existential type for polymorphic list
data RenderLayer = forall r . RenderLayerClass r => RenderLayer r
instance RenderLayerClass RenderLayer where
  texture (RenderLayer l) = texture l

-- | A 'Camera' is just a rectangle of view inside the world
type Camera = Maybe (SDL.Rectangle CInt)

-- | A 'Projector' is basically the same as a 'Camera'
type Projector = Camera

data Scene = Scene
    {
      sceneLayers :: [RenderLayer]
    , sceneCamera :: Camera
    }


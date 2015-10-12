-- |
-- Module:     FRP.Timeless.Framework.RPG.Render.Types
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>


module FRP.Timeless.Framework.RPG.Render.Types
       where

import qualified SDL as SDL
import Foreign.C.Types (CInt)
import Linear
import Linear.Affine

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

-- * Type related utilities

-- | Create a `SDL.Rectangle CInt`
cIntRect :: (Integral n) => Point V2 n -> V2 n -> SDL.Rectangle CInt
cIntRect pos@(P pv) size =
  SDL.Rectangle (P $ fmap toCInt pv) (fmap toCInt size)

-- | Convert an `Integral` rectangle to `CInt`
toCRect :: (Integral n) => SDL.Rectangle n -> SDL.Rectangle CInt
toCRect (SDL.Rectangle pos size) = cIntRect pos size

-- | Convert to CInt
toCInt :: (Integral n) => n -> CInt
toCInt = fromIntegral



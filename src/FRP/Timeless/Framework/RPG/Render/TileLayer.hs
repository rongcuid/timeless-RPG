-- | The tile layer renderer. Note: This code is really a mess,
-- I will fix this when I need to.
--
-- Module: FRP.Timeless.Framework.RPG.Render.TileLayer Copyright: (c)
-- 2015 Rongcui Dong License: BSD3 Maintainer: Rongcui Dong
-- <karl_1702@188.com>

module FRP.Timeless.Framework.RPG.Render.TileLayer
       (
         module Data.Tiled
       , TileRenderData(..)
       , blitTileLayer
       , loadTileRenderData
       , createLayerRenderer
       )
       where

import Prelude hiding ((.), id)
import qualified SDL as SDL
import Data.Tiled
import qualified Data.Map as Map
import Data.Map (Map, (!))
import Control.Monad.Reader
import Linear
import Linear.Affine
import GHC.Word
import Foreign.C.Types (CInt)
import Data.StateVar (($=))
import qualified SDL.Raw.Types as RAW

import FRP.Timeless
import FRP.Timeless.Framework.RPG.Render.Types

import qualified Debug.Trace as Debug

-- * Data Structures

-- ** Types

-- | Contains data necessary for render function to work
data TileRenderData = TileRenderData
    {
      rdRenderer :: SDL.Renderer
    , rdMapDesc :: TiledMap
    , rdSpriteSheet :: SpriteSheet
    , rdSprites :: [SDL.Surface]
    }

-- | Layer Renderer data, whose Texture is the final destination for this layer
type LayerRendererData = (TileRenderData, Layer, SDL.Texture)
instance RenderLayerClass LayerRendererData where
  texture = \(_,_,t) -> t

-- | A sprite contains the index of tileset and the rectangle on the
-- tileset texture
type Sprite = (Int, SDL.Rectangle Int)

-- | This is just the data section of layer
type LayerData = Map (Int, Int) Tile

-- | A sprite sheet uses GID as key, and Sprite as value
type SpriteSheet = Map Word32 Sprite

-- ** Factories

-- | Make a sprite sheet (map) out of a tile map
makeSpriteSheet :: TiledMap -> SpriteSheet
makeSpriteSheet tm = go 0 tss Map.empty
  where
    tss = mapTilesets tm
    go :: Int -> [Tileset] -> SpriteSheet -> SpriteSheet
    go tsID (ts:tss') ss =
        let gid0 = fromIntegral $ tsInitialGid ts
            nt = getNumTiles ts
            -- | [(gid, sprite)]
            sprites = [ (fromIntegral gid, (tsID, getRect ts gid0 gid)) |
                        gid <- [gid0..(gid0+nt-1)]
                      ]
        in go (tsID+1) tss' $ goInsert ss sprites
    go _ [] ss = ss
    -- | Insert the sprites using gid as key
    goInsert ss ((gid,sp):sps) = goInsert (Map.insert gid sp ss) sps
    goInsert ss [] = ss

-- * Rendering Functions

-- | Renders one tile
blitTile :: SDL.Surface
           -- ^ Destination surface
           -> SpriteSheet
           -> [SDL.Surface]
           -- ^ Sprite sheet surfaces
           -> LayerData
           -> V2 Int
           -- ^ Dimension of map in tiles
           -> V2 Int
           -- ^ Tile size
           -> Int
           -- ^ Index
           -> IO ()
blitTile dest ss surfs dat sz@(V2 w h) ts@(V2 tw th) idx = do
  let key@(xt,yt) = getLayerKey sz idx
      -- ^ Get the key for layer data
      mTile = Map.lookup key dat
      -- ^ Get the tile description
  case mTile of
    Just tile ->
      do
        let gid = tileGid tile -- Retrieve the sprite ID
        -- Sprite, texture index, source rectangle
        let mSp = Map.lookup gid ss
        case mSp of
          Just sprite@(i,rectS) ->
            do
              let rectSrc = toCRect rectS
              let pDest = P $ fmap toCInt $ V2 (xt*tw) (yt*th)
              SDL.surfaceBlit (surfs !! i) (Just rectSrc) dest (Just pDest)
          Nothing -> return ()
    Nothing -> return ()
  return ()

-- | Blits one tile map layer onto destination surface
blitTileLayer :: SDL.Surface -> TileRenderData -> Layer -> IO ()
blitTileLayer dest rd lay@(Layer _ _ _ _ _) = do
  let ss = rdSpriteSheet rd
      -- ^ Sprite sheet
      surfs = rdSprites rd
      -- ^ Sprite sheet surfaces
      (mapSizeT@(V2 w h), tileSize@(V2 tw th)) = getTMDimensions $ rdMapDesc rd
      -- ^ Some dimensions
      nt = w * h
      -- ^ Total number of tiles
      dat = layerData lay
  blitTile dest ss surfs dat mapSizeT tileSize `mapM_` [0..nt-1]
  
blitTileLayer _ _ _ = error "[TODO]: Only supports tile Tiled layer now"

-- * Constructing Renderer

-- ** Functions

-- | Loads a TileRenderData from file
loadTileRenderData :: SDL.Renderer -> FilePath -> IO TileRenderData
loadTileRenderData ren path = do
  -- v Load the map file
  tm <- loadMapFile path
  let ss = makeSpriteSheet tm
      -- ^ Create Sprite sheet
  let tss = mapTilesets tm
      -- ^ All Tilesets
  let tsPaths = (iSource . head . tsImages) <$> tss
      -- ^ Get all tileset image paths
  -- v Load all images to Surfaces
  surfs <- SDL.loadBMP `mapM` tsPaths
  -- v Make the TileRenderData
  return $ TileRenderData ren tm ss surfs

-- | Using TileRenderData, create a layer renderer
createLayerRenderer :: SDL.Renderer
                  -> Int
                  -> TileRenderData
                  -> IO LayerRendererData
createLayerRenderer ren i rd = do
  let (V2 w h, V2 tw th) = getTMDimensions $ rdMapDesc rd
      -- ^ Map dimensions
      wp = w * tw
      -- ^ Width in pixels
      hp = h * th
      -- ^ Height in pixels
      -- | The layer to be rendered
      lay = ((!! i) . mapLayers . rdMapDesc) rd
  let mask = V4 0xFF000000 0x00FF0000 0x0000FF00 0x000000FF
      bd = 32
  -- v First, make a software surface
  destSurf <- SDL.createRGBSurface (fmap toCInt $ V2 wp hp) bd mask
  -- v Then, blit the tilelayer on the surface
  blitTileLayer destSurf rd lay
  -- v Make a texture out of surface
  destTex <- SDL.createTextureFromSurface ren destSurf
  return (rd, lay, destTex)

-- * Utilities

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

-- | Gets X-Y coordinate from linear, row major, coordinates
idx2XY :: V2 Int
       -- ^ Size of map in tiles
       -> Int
       -- ^ Linear coordinate (Index)
       -- | XY coordinate
       -> V2 Int
idx2XY (V2 nx _) idx = V2 (idx `rem` nx) (idx `quot` nx)

-- | Gets a tuple for retrieving layer data. Same usage as `idx2XY`
getLayerKey :: V2 Int -> Int -> (Int, Int)
getLayerKey size idx = let V2 x y = idx2XY size idx in (x, y)

-- | A helper to get formatting information from tile set
getTSInfo :: Tileset -> (Int,Int,Int,Int)
getTSInfo ts =
    let tw = tsTileWidth ts
        th = tsTileHeight ts
        sp = tsSpacing ts
        mar = tsMargin ts
    in (tw,th,sp,mar)

-- | A helper to get dimension information of the tile map
getTMDimensions :: TiledMap -> (V2 Int, V2 Int)
getTMDimensions tm =
  let
    w = mapWidth tm
    h = mapHeight tm
    tw = mapTileWidth tm
    th = mapTileHeight tm
  in (V2 w h, V2 tw th)

-- | Get the size in tiles of a tile set
getTilesetSize :: Tileset -> V2 Int
getTilesetSize ts =
  let 
      (tw,th,sp,mar) = getTSInfo ts
      im:_ = tsImages ts
      iw = iWidth im
      ih = iHeight im
      nx = iw `quot` (tw+sp)
      ny = ih `quot` (th+sp)
  in V2 nx ny

-- | Get the total number of tiles in a tile set
getNumTiles :: Tileset -> Int
getNumTiles ts =
    let V2 nx ny = getTilesetSize ts
    in nx * ny

-- | Get a rectangle on texture containing the tile number using gid
getRect :: (Integral n) => Tileset -> n -> n -> SDL.Rectangle Int
getRect ts gid0 gid =
  let g' :: Int
      g' = fromIntegral (gid - gid0)
      V2 nx ny = getTilesetSize ts
      (tw,th,sp,mar) = getTSInfo ts
      tx = g' `rem` nx
      ty = g' `quot` nx
      cornerX = mar + tx * (sp+tw)
      cornerY = mar + ty * (sp+th)
  in SDL.Rectangle (P $ V2 cornerX cornerY) (V2 tw th)
      

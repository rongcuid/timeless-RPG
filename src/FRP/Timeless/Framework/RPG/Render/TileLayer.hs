-- |
-- Module:     FRP.Timeless.Framesork.RPG.Render.TileLayer
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Framework.RPG.Render.TileLayer
       (
         module Data.Tiled
       , TileRenderData(..)
       , renderTileLayer
       , loadTileRenderData
       , sLayerRenderer
       )
       where

import Prelude hiding ((.), id)
import FRP.Timeless
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

-- * Data Structures

-- ** Types

-- | Layer Renderer data, whose Texture and Renderer are the destination
type LayerRendererData = (SDL.Renderer, TileRenderData, Layer, SDL.Texture)

-- | Contains data necessary for render function to work
data TileRenderData = TileRenderData
    {
      rdRenderer :: SDL.Renderer
    , rdMapDesc :: TiledMap
    , rdSpriteSheet :: SpriteSheet
    , rdTextures :: [SDL.Texture]
    }

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
    go lay (ts:tss) ss =
        let gid0 = fromIntegral $ tsInitialGid ts
            nt = getNumTiles ts
            -- | [(gid, sprite)]
            sprites = [ (fromIntegral gid, (lay, getRect ts gid0 gid)) |
                        gid <- [gid0..(gid0+nt-1)]
                      ]
        in goInsert ss sprites
    go _ [] ss = ss
    -- | Insert the sprites using gid as key
    goInsert ss ((gid,sp):sps) = goInsert (Map.insert gid sp ss) sps
    goInsert ss [] = ss

-- * Rendering Functions

-- | Renders one tile
renderTile :: SDL.Renderer
           -> SpriteSheet
           -> [SDL.Texture]
           -- ^ Sprite sheet textures
           -> LayerData
           -> V2 Int
           -- ^ Dimension of map in tiles
           -> V2 Int
           -- ^ Tile size
           -> Int
           -- ^ Index
           -> IO ()
renderTile ren ss txs dat sz@(V2 w h) ts@(V2 tw th) idx = do
  let key@(xt,yt) = getLayerKey sz idx
      -- ^ Get the key for layer data
      tile = dat ! key
      -- ^ Get the tile description
      gid = tileGid tile
      -- ^ Retrieve the sprite ID
      sprite@(i, rectS) = ss ! gid
      -- ^ Sprite, texture index, source rectangle
      rectSrc = toCRect rectS
      rectDest = cIntRect (P $ V2 (xt*tw) (yt*th)) (V2 tw th)
  SDL.copy ren (txs !! i) (Just rectSrc) (Just rectDest)
  return ()

-- | Renders one tile map layer onto `ren`
renderTileLayer :: SDL.Renderer -> TileRenderData -> Layer -> IO ()
renderTileLayer ren rd lay@(Layer _ _ _ _ _) = do
  let ss = rdSpriteSheet rd
      -- ^ Sprite sheet
      txs = rdTextures rd
      -- ^ Sprite sheet textures
      (mapSizeT@(V2 w h), tileSize@(V2 tw th)) = getTMDimensions $ rdMapDesc rd
      -- ^ Some dimensions
      nt = w * h
      -- ^ Total number of tiles
      dat = layerData lay
  renderTile ren ss txs dat mapSizeT tileSize `mapM_` [0..nt]
  
renderTileLayer _ _ _ = error "Only supports tile layer"

-- * Constructing Renderer

-- ** Functions

-- | Loads a TileRenderData from file
loadTileRenderData :: SDL.Window -> FilePath -> IO TileRenderData
loadTileRenderData w path = do
  -- | Load the map file
  tm <- loadMapFile path
  -- | Create Sprite sheet
  let ss = makeSpriteSheet tm
  -- | All Tilesets
  let tss = mapTilesets tm
  -- | Get all tileset image paths
  let tsPaths = (iSource . head . tsImages) <$> tss
  -- | Load all images to Surfaces
  surfs <- SDL.loadBMP `mapM` tsPaths
  -- | Create the Renderer
  ren <- SDL.createRenderer w (-1) SDL.defaultRenderer
  -- | Load surfaces to Textures
  txs <- SDL.createTextureFromSurface ren `mapM` surfs
  -- | Make the TileRenderData
  return $ TileRenderData ren tm ss txs

-- | Using TileRenderData, create a layer renderer
layerRendererData :: SDL.Window
                  -> Int
                  -> TileRenderData
                  -> IO LayerRendererData
layerRendererData win i rd = do
  -- | Destination Renderer
  renDest <- SDL.createRenderer win (-1)
         (SDL.RendererConfig SDL.AcceleratedRenderer True)
  let (V2 w h, V2 tw th) = getTMDimensions $ rdMapDesc rd
      -- ^ Map dimensions
      wp = w * tw
      -- ^ Width in pixels
      hp = h * th
      -- ^ Height in pixels
      -- | The layer to be rendered
      lay = ((!! i) . mapLayers . rdMapDesc) rd
  pFmt <- SDL.getWindowPixelFormat win
  destTex <- SDL.createTexture renDest pFmt SDL.TextureAccessTarget
             (fmap fromIntegral $ V2 wp hp)
  SDL.rendererRenderTarget renDest $= Just destTex
  return (renDest, rd, lay, destTex)

-- | Run a layer renderer
runLayerRenderer :: LayerRendererData -> IO SDL.Texture
runLayerRenderer (ren,rd,lay,tex) = do
  renderTileLayer ren rd lay
  return tex


-- ** Signal

-- | The signal that runs a layer renderer
sLayerRenderer :: SDL.Window
               -> Int
               -- ^ The layer index
               -> Signal s IO TileRenderData SDL.Texture
sLayerRenderer w i = lrd >>> mkKleisli_ runLayerRenderer
    where
      lrd = mkKleisli_ $ layerRendererData w i 

-- * Utilities

-- | Create a `SDL.Rectangle CInt`
cIntRect :: (Integral n) => Point V2 n -> V2 n -> SDL.Rectangle CInt
cIntRect pos@(P pv) size =
  SDL.Rectangle (P $ fmap fromIntegral pv) (fmap fromIntegral size)

-- | Convert an `Integral` rectangle to `CInt`
toCRect :: (Integral n) => SDL.Rectangle n -> SDL.Rectangle CInt
toCRect (SDL.Rectangle pos size) = cIntRect pos size

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
      

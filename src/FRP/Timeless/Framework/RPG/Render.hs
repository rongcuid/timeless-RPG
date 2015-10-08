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
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Reader
import Linear
import Linear.Affine
import GHC.Word

-- renderMap :: SDL.Texture -> TiledMap -> IO ()
-- renderMap r m = do
--   return ()

-- | A sprite contains the index of tileset and the rectangle on the
-- tileset texture
type Sprite = (Int, SDL.Rectangle Int)

-- | A sprite sheet uses GID as key, and Sprite as value
type SpriteSheet = Map Int Sprite

-- | A helper to get useful information from tile set
getTSInfo ts =
    let tw = tsTileWidth ts
        th = tsTileHeight ts
        sp = tsSpacing ts
        mar = tsMargin ts
    in (tw,th,sp,mar)

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

-- | Get a rectangle containing the tile number gid
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
      
-- | Make a sprite sheet (map) out of a tile map
makeSpriteSheet :: TiledMap -> SpriteSheet
makeSpriteSheet tm = go 0 tss Map.empty
  where
    tss = mapTilesets tm
    go :: Int -> [Tileset] -> Map Int Sprite -> SpriteSheet
    go lay (ts:tss) ss =
        let gid0 = fromIntegral $ tsInitialGid ts
            nt = getNumTiles ts
            -- | [(gid, sprite)]
            sprites = [ (gid, (lay, getRect ts gid0 gid)) |
                        gid <- [gid0..(gid0+nt-1)]
                      ]
        in goInsert ss sprites
    go _ [] ss = ss
    -- | Insert the sprites using gid as key
    goInsert ss ((gid,sp):sps) = goInsert (Map.insert gid sp ss) sps
    goInsert ss [] = ss


-- | Look up a sprite using GID
getTile :: Int -> ReaderT SpriteSheet IO (Maybe Sprite)
getTile gid = ask >>= return . (Map.lookup gid)

-- | Renders one tile map layer onto `ren`
renderTileLayer :: SDL.Renderer -> Layer -> ReaderT (TiledMap) IO ()
renderTileLayer ren lay@(Layer _ _ _ _ _) = do
  tm <- ask 
  let dat = layerData lay
  return ()

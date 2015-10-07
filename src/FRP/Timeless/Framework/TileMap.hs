-- | This module defines the data structure of the game tilemap, and
-- provides functions to read from tilemap data files
-- 
-- Module:     FRP.Timeless.Framework.TileMap
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

{-# LANGUAGE TemplateHaskell #-}
module FRP.Timeless.Framework.TileMap where

import Control.Lens

type Position = (Int, Int)

-- | Description of tile set
data TileSetDesc = TileSetDesc
               { _tileSetName :: String
               , _tileSetGridSize :: (Int, Int)
                 -- ^ Grid size in pixels
               }
                 deriving (Show)

-- | Description of one tile map layer
data TileLayerDesc = TileLayerDesc
                 { _tileLayerName :: String
                 , _tileLayerTileSet :: TileSetDesc
                 , _tileLayerSize :: (Int, Int)
                   -- ^ Layer size in grids
                 , _tileLayerOffset :: Position
                   -- ^ Layer offset in grids
                 , _tileLayerDesc :: [Int]
                 }
                   deriving (Show)

-- | Description of one tile map
data TileMapDesc = TileMapDesc
               { _tileMapName :: String
               , _tileMapDesc :: [TileLayerDesc]
               }
                 deriving (Show)

makeLenses ''TileSetDesc
makeLenses ''TileLayerDesc
makeLenses ''TileMapDesc

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
type TileDesc = (String, Int)
-- ^ Description of one tile: (TileSetName, Index)

data TileLayer = TileLayer
                 { _tileLayerName :: String
                 , _tileLayerSize :: (Int, Int)
                 , _tileLayerOffset :: Position
                 , _tileLayerDesc :: [TileDesc]
                 }

makeLenses ''TileLayer

data TileMap = TileMap
               { _tileMapName :: String
               , _tileMapSize :: (Int, Int)
               , _tileMapOffset :: [TileLayer]
               }

makeLenses ''TileMap

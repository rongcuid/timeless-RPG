-- |
-- Module:     FRP.Timeless.Framesork.RPG.Render
-- Copyright:  (c) 2015 Rongcui Dong
-- License:    BSD3
-- Maintainer: Rongcui Dong <karl_1702@188.com>

module FRP.Timeless.Framework.RPG.Render where

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

import FRP.Timeless
import FRP.Timeless.Framework.RPG.Render.TileLayer

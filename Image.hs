module Image where

import Codec.Picture
import qualified Data.Vector.Storable as V

import Util

image :: Camera -> [Color] -> Image PixelRGB8
image (Camera w h _) pixels = Image w h $ V.fromList $ idata pixels
    where idata = concat . map toList
          toList (Color r g b) = [toByte r, toByte g, toByte b]
          toByte = truncate . (255 *) . min 1.0 . max 0.0

save filename camera colors = writePng filename $ image camera colors

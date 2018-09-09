module Image where

import Codec.Picture
import qualified Data.Vector.Storable as V

import Util
import Vector

image :: Camera -> [Vector] -> Image PixelRGB8
image (Camera w h _) pixels = Image w h $ V.fromList $ idata pixels
    where idata = concat . map toList
          toList (Vector color) = map (toByte . sqrt) color
          toByte = truncate . (255.99 *) . min 1.0 . max 0.0

save filename camera colors = writePng filename $ image camera colors

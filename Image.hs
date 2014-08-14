import Codec.Picture

import qualified Data.Vector.Storable as V

data Camera = Camera {width :: Int, height :: Int, f :: Double}
data Color = Color {red :: Double, green :: Double, blue :: Double}

test = replicate 10000 $ Color 1.0 0.0 0.0

image :: Camera -> [Color] -> Image PixelRGB8
image (Camera w h _) pixels = Image w h $ V.fromList $ idata pixels
    where idata = concat . map toList
          toList (Color r g b) = [toByte r, toByte g, toByte b]
          toByte = truncate . (255 *) . min 1.0 . max 0.0
          
main = writePng "output.png" $ image (Camera 100 100 8) test


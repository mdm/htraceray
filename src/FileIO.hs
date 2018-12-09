module FileIO where

import Codec.Picture
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V

import Util
import Vector
import Object

image :: Camera -> [Vector] -> Image PixelRGB8
image (Camera w h _ _ _ _) pixels = Image w h $ VS.fromList $ idata pixels
    where idata = concat . map toList
          toList (Vector color) = map (toByte . sqrt) color
          toByte = truncate . (255.99 *) . min 1.0 . max 0.0

save filename camera colors = writePng filename $ image camera colors

readObjFile path material = do
                      contents <- readFile path
                      return $ readObjFile' contents
                  --     return $ makeVertices2 (extractLines 'v' contents)
    where extractLines lineType contents = filter (\line -> head line == lineType) $ lines contents
          makeVertices lines = V.fromList $ map ((multiplyscalar 1) . Vector . (map read) . tail . words) lines
          makeVertices2 lines = map ((multiplyscalar 1) . Vector . (map read) . tail . words) lines
          makeFaces vertices lines = map ((flip Triangle material) . (map ((V.!) vertices . (\i -> i - 1) . read)) . tail . words) lines
          readObjFile' contents = makeFaces (makeVertices (extractLines 'v' contents)) (extractLines 'f' contents)

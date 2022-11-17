module FileIO where

import Codec.Picture
import Data.List (isPrefixOf)
import Data.List.Split
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
    where extractLines lineType contents = filter (\line -> lineType `isPrefixOf` line) $ lines contents
          makeVectors ls = V.fromList $ map ((multiplyscalar 1) . Vector . (map read) . tail . words) ls
          vertices contents = makeVectors (extractLines "v" contents)
          normals contents = makeVectors (extractLines "vn" contents)
          faceVertices vs = map ((V.!) vs . (\i -> i - 1) . read . (!!0) . (splitOn "/"))
          faceNormals ns = map (fmap ((V.!) ns . (\i -> i - 1) . read) . flip maybeAt 2 . (splitOn "/"))
          zipVsAndNs vs ns fs = (faceVertices vs fs, faceNormals ns fs)
          makeFaces vs ns = map ((\(v, n) -> Triangle v n material) . (zipVsAndNs vs ns) . tail . words)
          readObjFile' contents = makeFaces (vertices contents) (normals contents) (extractLines "f" contents)

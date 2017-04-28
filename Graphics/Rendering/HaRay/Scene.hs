module Graphics.Rendering.HaRay.Scene where

import Graphics.Rendering.HaRay.Base
import Graphics.Rendering.HaRay.Camera

import Codec.Picture

intersect :: Scene -> Ray -> Maybe (Entity, RaycastHit)
intersect Scene{..} ray = doRaycast entities
  where
    doRaycast [] = Nothing
    doRaycast (e:es) = maybe (doRaycast es) compareHits $ geometry e (transform e) ray
      where
        compareHits hit =
          let thisRes = Just (e, hit) in
          case doRaycast es of
            nextRes@(Just (_, hit')) -> if hitDistance hit' < hitDistance hit then nextRes else thisRes
            Nothing -> thisRes

rayTrace :: Scene -> Ray -> Color
rayTrace scene@(Scene{..}) ray =
  case intersect scene ray of
    Nothing -> background camera
    Just (e, hit) -> saturate $ material e scene hit

renderScene :: Scene -> String -> IO ()
renderScene scene@(Scene{..}) path =
  let image = generateImage pixelRenderer (round width) (round height) in
  writePng path image
  where
    pixelRenderer x y =
      let u = fromIntegral x / width
          v = 1 - fromIntegral y / height in
      toPixelRGB8 $ rayTrace scene $ generateRay camera aspectRatio u v

    width = screenWidth settings
    height = screenHeight settings
    aspectRatio = width / height
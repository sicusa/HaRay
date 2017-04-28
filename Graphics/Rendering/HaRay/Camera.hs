module Graphics.Rendering.HaRay.Camera where

import Graphics.Rendering.HaRay.Base

mkCamera :: Vector3D -> Vector3D -> Vector3D -> Float -> Color -> Camera
mkCamera position front refUp fov background =
  Camera { eye = position
         , front = front
         , right = right
         , up    = right `cross` front
         , fovScale = 2 * tan (fov * 0.5 * pi / 180)
         , background = background}
  where
    right = front `cross` refUp

generateRay :: Camera -> Float -> Float -> Float -> Ray
generateRay Camera{..} aspectRatio x y =
  Ray { origin = eye
      , direction = normalize $ front + r + u
      , reflectCount = 0 }
  where
    r = right <*# ((x - 0.5) * aspectRatio * fovScale)
    u = up <*# ((y - 0.5) * fovScale)
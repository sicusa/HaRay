module Graphics.Rendering.HaRay.Base.Ray where

import Graphics.Rendering.HaRay.Base.Vector3D

data Ray = Ray
  { origin       :: Vector3D
  , direction    :: Vector3D
  , reflectCount :: Int }
  deriving (Show)

reflectRay :: Vector3D -> Vector3D -> Ray -> Ray
reflectRay origin normal ray =
  Ray { origin = origin
      , direction = reflect (direction ray) normal
      , reflectCount = reflectCount ray + 1 }

rayPoint :: Float -> Ray -> Vector3D
rayPoint t (Ray {..}) = origin + direction <*# t

data RaycastHit = RaycastHit
  { hitRay       :: Ray
  , hitDistance  :: Float
  , hitPosition  :: Vector3D
  , hitNormal    :: Vector3D }
  deriving (Show)
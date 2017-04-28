module Graphics.Rendering.HaRay.Entity where

import Graphics.Rendering.HaRay.Base
import Graphics.Rendering.HaRay.Geometry
import Graphics.Rendering.HaRay.Material

mkEntity :: Vector3D -> Geometry -> Material -> Entity
mkEntity pos geometry material = Entity
  { transform = Transform { position = pos, forward = upDir }
  , geometry  = geometry
  , material  = material
  , castShadow = True }
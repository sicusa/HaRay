module Graphics.Rendering.HaRay.Base.Transform where

import Graphics.Rendering.HaRay.Base.Vector3D

data Transform = Transform
  { position :: Vector3D
  , forward  :: Vector3D }


module Graphics.Rendering.HaRay.Geometry.Primitives where

import Graphics.Rendering.HaRay.Base

mkSphere :: Float -> Geometry
mkSphere radius Transform{..} ray =
  if ddv > 0 || discr < 0
    then Nothing
    else Just hitRes
  where
    v = origin ray - position
    ddv = direction ray <.> v
    discr = ddv * ddv - (v <.> v - radius * radius)

    hitDis  = -ddv - sqrt discr
    hitPos = rayPoint hitDis ray
    hitRes = RaycastHit
      { hitRay      = ray
      , hitDistance = hitDis
      , hitPosition = hitPos
      , hitNormal   = normalize $ hitPos - position }

mkPlane :: Geometry
mkPlane Transform{..} ray =
  if ddn >= 0
    then Nothing
    else Just hitRes
  where
    ddn = direction ray <.> forward
    hitDis = (compY position - forward <.> origin ray) / ddn
    hitRes = RaycastHit
      { hitRay      = ray
      , hitDistance = hitDis
      , hitPosition = rayPoint hitDis ray
      , hitNormal   = forward }
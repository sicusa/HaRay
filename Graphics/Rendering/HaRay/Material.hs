{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Graphics.Rendering.HaRay.Material where

import Graphics.Rendering.HaRay.Base
import Graphics.Rendering.HaRay.Scene

instance Num Material where
  m1 + m2 = \scene raycastHit -> m1 scene raycastHit + m2 scene raycastHit
  m1 - m2 = \scene raycastHit -> m1 scene raycastHit + m2 scene raycastHit
  m1 * m2 = \scene raycastHit -> m1 scene raycastHit * m2 scene raycastHit
  negate m = \scene raycastHit -> -(m scene raycastHit)
  abs m = \scene raycastHit -> abs $ m scene raycastHit
  signum m = \scene raycastHit -> signum $ m scene raycastHit
  fromInteger i = matAmbient $ fromInteger i

matDepthDisplay :: Float -> Material
matDepthDisplay maxDepth _ RaycastHit{..} =
  let depth = 255 - round (min 255 $ hitDistance / maxDepth * 255) in
  rgb depth depth depth

matNormalDisplay :: Material
matNormalDisplay _ RaycastHit{..} =
  let cv = (hitNormal + 1) * 128 in
  rgb (round $ compX cv) (round $ compY cv) (round $ compZ cv)

matChecker :: Float -> Material
matChecker scale _ RaycastHit{..} =
  let t = floor (scale * compX hitPosition) + floor (scale * compZ hitPosition) in
  if abs (t `mod` 2) < 1 then black else white

matAmbient :: Color -> Material
matAmbient color _ _ = color

matReflect :: Float -> Material
matReflect reflexivity scene RaycastHit{..} =
  if reflexivity > 0 && reflectCount hitRay < maxReflect (settings scene)
    then reflexivity #*. rayTrace scene (reflectRay hitPosition hitNormal hitRay)
    else black

matLit :: Bool -> Material
matLit receiveShadow scene RaycastHit{..} =
  foldr lightAccumulator black (lights scene)
  where
    lightAccumulator light irr =
      let sample = sampleLight light scene receiveShadow hitPosition in
      if sampleIrradiance sample /= black
        then irr + surfaceIrradiance hitNormal sample
        else irr
        
matSpecular :: Int -> Material
matSpecular shininess Scene{..} RaycastHit{..} =
  sum $ map (\l -> specularFactor l #*. specular l) lights
  where
    specularFactor light = 
      let lightDir = normalize $ position (source light) - hitPosition
          h = normalize $ cameraDir + lightDir in
      (max (h <.> hitNormal) 0) ^ shininess
    cameraDir = normalize $ eye camera - hitPosition
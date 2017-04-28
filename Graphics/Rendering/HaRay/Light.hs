module Graphics.Rendering.HaRay.Light where

import Graphics.Rendering.HaRay.Base
import Graphics.Rendering.HaRay.Scene

import Data.Maybe

simpleShadow :: Scene -> LightSample -> LightSample
simpleShadow scene sample@(LightSample{..}) =
  maybe sample (const zeroLightSample) $ intersect scene $ Ray samplePosition sampleDirection 0

simpleShadowWithin :: Float -> Scene -> LightSample -> LightSample
simpleShadowWithin dis scene sample@(LightSample{..}) =
  let hitRes = intersect scene $ Ray samplePosition sampleDirection 0 in
  case hitRes of
    Just (_, hit) -> if hitDistance hit < dis then zeroLightSample else sample
    Nothing  -> sample

mkDirectionalSampler :: LightSampler
mkDirectionalSampler Light{..} scene enableShadow pos =
  let dir = negate $ forward source
      sample = LightSample pos dir irradiance in
  if castShadow && enableShadow
    then simpleShadow scene sample
    else sample

mkPointSampler :: LightSampler
mkPointSampler Light{..} scene enableShadow pos =
  let v = position source - pos
      dis = norm2 v
      sample = LightSample pos (normalize v) $ irradiance ./# (dis * dis) in
  if castShadow && enableShadow
    then simpleShadowWithin dis scene sample
    else sample
module Graphics.Rendering.HaRay.Base.Types where

import Graphics.Rendering.HaRay.Base.Ray
import Graphics.Rendering.HaRay.Base.Color
import Graphics.Rendering.HaRay.Base.Vector3D
import Graphics.Rendering.HaRay.Base.Transform

data Scene = Scene
  { settings :: RenderSettings
  , camera   :: Camera
  , entities :: [Entity]
  , lights   :: [Light] }

data RenderSettings = RenderSettings
  { screenWidth  :: Float
  , screenHeight :: Float
  , maxReflect   :: Int }

data Camera = Camera
  { eye        :: Vector3D
  , front      :: Vector3D
  , right      :: Vector3D
  , up         :: Vector3D
  , fovScale   :: Float
  , background :: Color }

data Entity = Entity
  { transform :: Transform
  , geometry  :: Geometry
  , material  :: Material
  , castShadow :: Bool }

type Geometry = Transform -> Ray -> Maybe RaycastHit
type Material = Scene -> RaycastHit -> Color

data Light = Light
  { source     :: Transform
  , irradiance :: Color
  , specular   :: Color
  , sampler    :: LightSampler
  , castShadow :: Bool }

sampleLight :: Light -> Scene -> Bool -> Vector3D -> LightSample
sampleLight light enableShadow scene pos = sampler light light enableShadow scene pos

type LightSampler = Light -> Scene -> Bool -> Vector3D -> LightSample

data LightSample = LightSample
  { samplePosition   :: Vector3D
  , sampleDirection  :: Vector3D
  , sampleIrradiance :: Color }

zeroLightSample :: LightSample
zeroLightSample = LightSample 0 0 black

surfaceIrradiance :: Vector3D -> LightSample -> Color
surfaceIrradiance normal LightSample{..} =
  sampleIrradiance .*# max (normal <.> sampleDirection) 0

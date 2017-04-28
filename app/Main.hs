module Main where

import Graphics.Rendering.HaRay

main :: IO ()
main = renderScene scene "./result.png"
  where
    scene = Scene
      { settings = RenderSettings
          { screenWidth  = 512
          , screenHeight = 512
          , maxReflect   = 3 }

      , camera = mkCamera (Vector3D 0 3 7) (normalize $ Vector3D 0 (-0.2) (-1)) upDir 60 black

      , entities =
          [ sphereEntity1
          , sphereEntity2
          , planeEntity ]

      , lights =
          [ Light
              { source = Transform (Vector3D 0 5 1) 0
              , irradiance = white .*# 30
              , specular = white ./# 2
              , sampler = mkPointSampler
              , castShadow = True }
          ] }

   {-
    myPhongLighting = PhongLighting
      { ambient     = rgb 100 100 100
      , diffuse     = blue
      , specular    = white
      , shininess   = 15
      , reflexivity = 0.3 } -}
    
    matBlinnPhong = matReflect 0.3
                  + matAmbient (blue ./# 6)
                  + ( matAmbient (blue ./# 2)
                    + matSpecular 30 * matAmbient white
                    ) * matLit True
    matTest = matSpecular 5 * matAmbient (white ./# 2)

    sphereEntity1 = mkEntity (Vector3D (-1.5) 1 0) (mkSphere 1) matBlinnPhong
    sphereEntity2 = mkEntity (Vector3D 1.5 1 0) (mkSphere 1) matBlinnPhong
    planeEntity = mkEntity 0 mkPlane $ matReflect 0.9 + (matChecker 1) * matLit True
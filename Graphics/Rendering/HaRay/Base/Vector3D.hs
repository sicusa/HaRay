{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Graphics.Rendering.HaRay.Base.Vector3D where

import Data.Data
import GHC.Generics

data Vector3D = Vector3D Float Float Float
  deriving (Show, Read, Eq, Data, Generic)

compX :: Vector3D -> Float
compX (Vector3D x _ _) = x

compY :: Vector3D -> Float
compY (Vector3D _ y _) = y

compZ :: Vector3D -> Float
compZ (Vector3D _ _ z) = z

instance Num Vector3D where
  (Vector3D x1 y1 z1) + (Vector3D x2 y2 z2) = Vector3D (x1 + x2) (y1 + y2) (z1 + z2)
  (Vector3D x1 y1 z1) - (Vector3D x2 y2 z2) = Vector3D (x1 - x2) (y1 - y2) (z1 - z2)
  (Vector3D x1 y1 z1) * (Vector3D x2 y2 z2) = Vector3D (x1 * x2) (y1 * y2) (z1 * z2)
  negate (Vector3D x y z) = Vector3D (-x) (-y) (-z)
  abs (Vector3D x y z) = Vector3D (abs x) (abs y) (abs z)
  signum (Vector3D x y z) = Vector3D (signum x) (signum y) (signum z)
  fromInteger i = let a = fromInteger i in Vector3D a a a

(<.>) :: Vector3D -> Vector3D -> Float
(Vector3D x1 y1 z1) <.> (Vector3D x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

(<+#) :: Vector3D -> Float -> Vector3D
(Vector3D x y z) <+# f = Vector3D (x + f) (y + f) (z + f)

(#+>) :: Float -> Vector3D -> Vector3D
f #+> (Vector3D x y z) = Vector3D (x + f) (y + f) (z + f)

(<*#) :: Vector3D -> Float -> Vector3D
(Vector3D x y z) <*# f = Vector3D (x * f) (y * f) (z * f)

(#*>) :: Float -> Vector3D -> Vector3D
f #*> (Vector3D x y z) = Vector3D (x * f) (y * f) (z * f)

(</#) :: Vector3D -> Float -> Vector3D
(Vector3D x y z) </# f = Vector3D (x / f) (y / f) (z / f)

(#/>) :: Float -> Vector3D -> Vector3D
f #/> (Vector3D x y z) = Vector3D (x / f) (y / f) (z / f)

infixl 6 <+#
infixl 6 #+>

infixl 7 <.>
infixl 7 <*#
infixl 7 #*>
infixl 7 </# 
infixl 7 #/>

upDir :: Vector3D
upDir = Vector3D 0 1 0

downDir :: Vector3D
downDir = Vector3D 0 (-1) 0

leftDir :: Vector3D
leftDir = Vector3D (-1) 0 0

rightDir :: Vector3D
rightDir = Vector3D 1 0 0

forwardDir :: Vector3D
forwardDir = Vector3D 0 0 1

backwardDir :: Vector3D
backwardDir = Vector3D 0 0 (-1)

norm1 :: Vector3D -> Float
norm1 (Vector3D x y z) = x + y + z

norm2 :: Vector3D -> Float
norm2 (Vector3D x y z) = sqrt $ x * x + y * y + z * z

normInf :: Vector3D -> Float
normInf (Vector3D x y z) = max (abs x) $ max (abs y) (abs z)

normalize :: Vector3D -> Vector3D
normalize v = v </# norm2 v

cross :: Vector3D -> Vector3D -> Vector3D
cross (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) =
  Vector3D (y1 * z2 - y2 * z1) (z1 * x2 - z2 * x1) (x1 * y2 - x2 * y1)

reflect :: Vector3D -> Vector3D -> Vector3D
reflect v norm = v - 2 * (v <.> norm) #*> norm

distance :: Vector3D -> Vector3D -> Float
distance (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) =
  sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^2 + (z1 - z2) ^ 2
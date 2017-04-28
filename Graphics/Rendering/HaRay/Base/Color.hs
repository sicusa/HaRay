{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Graphics.Rendering.HaRay.Base.Color where

import Codec.Picture
import Data.Data
import GHC.Generics

data Color = Color Float Float Float Float
  deriving (Show, Read, Eq, Data, Generic)

instance Bounded Color where
  minBound = black
  maxBound = white

instance Num Color where
  (Color r1 g1 b1 a1) + (Color r2 g2 b2 a2) = Color (r1 + r2) (g1 + g2) (b1 + b2) (a1 + a2)
  (Color r1 g1 b1 a1) - (Color r2 g2 b2 a2) = Color (r1 - r2) (g1 - g2) (b1 - b2) (a1 - a2)
  (Color r1 g1 b1 a1) * (Color r2 g2 b2 a2) = Color (r1 * r2) (g1 * g2) (b1 * b2) (a1 * a2)
  negate (Color r g b a) = Color (-r) (-g) (-b) (-a)
  abs (Color r g b a) = Color (abs r) (abs g) (abs b) (abs a)
  signum (Color r g b a) = Color (signum r) (signum g) (signum b) (signum a)
  fromInteger i = let a = fromInteger i / 255 in Color a a a a
  
(.*#) :: Color -> Float -> Color
(.*#) (Color r g b a) f = Color (r * f) (g * f) (b * f) (a * f)

(#*.) :: Float -> Color -> Color
(#*.) f (Color r g b a) = Color (r * f) (g * f) (b * f) (a * f) 

(./#) :: Color -> Float -> Color
(./#) (Color r g b a) f = Color (r / f) (g / f) (b / f) (a / f)

(#/.) :: Float -> Color -> Color
(#/.) f (Color r g b a) = Color (r / f) (g / f) (b / f) (a / f) 

infixl 7 .*#
infixl 7 #*.
infixl 7 ./#
infixl 7 #/.

rgb :: Word -> Word -> Word -> Color
rgb r g b = Color (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) 1

rgba :: Word -> Word -> Word -> Word -> Color
rgba r g b a = Color (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) (fromIntegral a / 255)

toPixelRGB8 :: Color -> PixelRGB8
toPixelRGB8 (Color r g b a) = PixelRGB8 (round $ r * 255) (round $ g * 255) (round $ b * 255)

toPixelRGBA8 :: Color -> PixelRGBA8
toPixelRGBA8 (Color r g b a) = PixelRGBA8 (round $ r * 255) (round $ g * 255) (round $ b * 255) (round $ a * 255)

white :: Color
white = rgb 255 255 255

black :: Color
black = rgb 0 0 0

gray :: Color
gray = rgb 44 44 44

red :: Color
red = rgb 255 0 0

green :: Color
green = rgb 0 255 0

blue :: Color
blue = rgb 0 0 255

saturate :: Color -> Color
saturate (Color r g b a) = Color (clampFloat r) (clampFloat g) (clampFloat b) (clampFloat a)
  where clampFloat v = max 0 (min 1 v)
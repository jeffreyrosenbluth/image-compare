{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Main where

import Data.Vector.Storable as V
import Codec.Picture
import Codec.Picture.Types (promoteImage)

zipPixelComponent
    :: forall px. ( V.Storable (PixelBaseComponent px))
    => (PixelBaseComponent px -> PixelBaseComponent px -> PixelBaseComponent px)
    -> Image px -> Image px -> Image px
{-# INLINE zipPixelComponent #-}
zipPixelComponent f i1@(Image { imageWidth = w1, imageHeight = h1 }) 
                    i2@(Image { imageWidth = w2, imageHeight = h2 })
  | w1 /= w2 || h1 /= h2 = error "Different image sizes"
  | otherwise       = Image { imageWidth = w1
                            , imageHeight = h2
                            , imageData = V.zipWith f data1 data2 }
  where 
    data1 = imageData i1
    data2 = imageData i2

foldPixelComponent :: Storable (PixelBaseComponent a) 
                   => (b -> PixelBaseComponent a -> b) -> b -> Image a -> b
foldPixelComponent f acc im =  V.foldl' f acc (imageData im)

pixelDiff :: (Storable (PixelBaseComponent px), Num (PixelBaseComponent px)
             ,Ord (PixelBaseComponent px))
          => Image px -> Image px -> Image px
pixelDiff i1 i2 = zipPixelComponent sub i1 i2
  where
    -- Must be careful to substract Word8 types.
    sub x y = max x y - min x y

imageDiff :: (Storable (PixelBaseComponent a), Integral (PixelBaseComponent a)
             ,Floating b)
          => Image a -> Image a -> b
imageDiff i1 i2 = sqrt $ foldPixelComponent f 0 (pixelDiff i1 i2) / n
  where
    n = fromIntegral $ imageWidth i1 * imageHeight i2
    f acc x = acc + fromIntegral  x * fromIntegral x 

-- Does not handle ImageYF and ImageRGBF, since Float is not Integral.
dynamicImageDiff :: Floating a => DynamicImage -> DynamicImage -> a
dynamicImageDiff (ImageY8 i1)     (ImageY8 i2)     = imageDiff i1 i2
dynamicImageDiff (ImageY16 i1)    (ImageY16 i2)    = imageDiff i1 i2
dynamicImageDiff (ImageYA8 i1)    (ImageYA8 i2)    = imageDiff i1 i2
dynamicImageDiff (ImageYA16 i1)   (ImageYA16 i2)   = imageDiff i1 i2
dynamicImageDiff (ImageRGB8 i1)   (ImageRGB8 i2)   = imageDiff i1 i2
dynamicImageDiff (ImageRGB16 i1)  (ImageRGB16 i2)  = imageDiff i1 i2
dynamicImageDiff (ImageRGBA8 i1)  (ImageRGBA8 i2)  = imageDiff i1 i2
dynamicImageDiff (ImageRGBA16 i1) (ImageRGBA16 i2) = imageDiff i1 i2
dynamicImageDiff (ImageYCbCr8 i1) (ImageYCbCr8 i2) = imageDiff i1 i2
dynamicImageDiff (ImageCMYK8 i1)  (ImageCMYK8 i2)  = imageDiff i1 i2
dynamicImageDiff (ImageRGBA8 i1)  (ImageRGB8 i2)   = imageDiff i1 (promoteImage i2)
dynamicImageDiff (ImageRGB8 i1)   (ImageRGBA8 i2)  = imageDiff (promoteImage i1) i2
dynamicImageDiff _                _                = 
  error "Different or unsupported pixel types"

main = do 
  di1 <- readImage "./orangeR.png"
  di2 <- readImage "./orangeC.png"
  let d = case (di1, di2) of
            (Left _, _) -> error "Image 1 not read"
            (_, Left _) -> error "Image 2 not read"
            (Right  i1, Right i2) -> dynamicImageDiff i1 i2
  print d


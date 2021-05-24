module Main where

import           System.Console.ANSI            ( setSGRCode )
import           System.Console.ANSI.Types      ( ConsoleIntensity
                                                  ( BoldIntensity
                                                  )
                                                , SGR(SetConsoleIntensity)
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure )

import           Data.Colour                    ( Colour )
import           Data.Colour.CIE                ( cieLABView
                                                , cieXYZView
                                                )
import           Data.Colour.CIE.Illuminant     ( d65 )
import           Data.Colour.RGBSpace.HSL       ( hslView )
import           Data.Colour.RGBSpace.HSV       ( hsvView )
import           Data.Colour.SRGB               ( RGB(..)
                                                , sRGB24
                                                , sRGB24read
                                                , sRGB24show
                                                , toSRGB
                                                , toSRGB24
                                                )
import           Data.Function                  ( on )
import           Data.List                      ( minimumBy )
import           Data.Word                      ( Word8 )
import           Text.Printf                    ( printf )

--
--
type Shade = Colour Float

--
--
main :: IO ()
main = do
  args <- getArgs
  if length args /= 1 then usage else printDetails $ head args

--
--
usage :: IO ()
usage = putStrLn "Usage: shades [#]RRGGBB"

--
--
printDetails :: String -> IO ()
printDetails hexColor = printShade $ parseShade hexColor

--
--
parseShade :: String -> Maybe Shade
parseShade s | length s == 6 || length s == 7 = Just (sRGB24read s)
             | otherwise                      = Nothing


-- 
-- 
printShade :: Maybe Shade -> IO ()
printShade Nothing = do
  putStrLn "failed to parse color!"
  exitFailure
printShade (Just c) = do
  putStrLn "\nShade details:"
  printf "%12s %s\n" "hex" (ansiBold ++ sRGB24show c ++ ansiReset)
  printf "%12s (%3d, %3d, %3d)\n"
         "c-style"
         (channelRed rgb)
         (channelGreen rgb)
         (channelBlue rgb)
  printf "%12s (%f, %f, %f)\n"
         "float"
         (channelRed rgb')
         (channelGreen rgb')
         (channelBlue rgb')
  printf "%12s (h: %f, s: %f, l: %f)\n"       "hsl" h  s  l
  printf "%12s (h: %f, s: %f, v: %f)\n"       "hsv" h' s' v
  printf "%12s (l: %f, a: %f, b: %f)\n"       "lab" l' a  b
  printf "%12s (x: %.4f, y: %.4f, z: %.4f)\n" "xyz" x  y  z
  printf "\nClosest ANSI Color:\n"
  printf "%12s (code: %d, hex: %s)\n"
         "rgb-euclid"
         rgbeCode
         (sRGB24show rgbeShade)
  printf "%12s (code: %d, hex: %s)\n"
         "rgb-weight"
         rgbwCode
         (sRGB24show rgbwShade)
  printf "%12s (code: %d, hex: %s)\n" "cie76" cie76Code (sRGB24show cie76Shade)
 where
  rgb                     = toSRGB24 c            --   0 <-> 255
  rgb'                    = toSRGB c :: RGB Float -- 0.f <-> 1.f
  (h , s , l)             = hslView rgb'
  (h', s', v)             = hsvView rgb'
  (l', a , b)             = cieLABView d65 c
  (x , y , z)             = cieXYZView c
  (rgbeCode , rgbeShade ) = findClosestANSIColor c squaredDistanceSRGB
  (rgbwCode , rgbwShade ) = findClosestANSIColor c weightedSquareDistanceSRGB
  (cie76Code, cie76Shade) = findClosestANSIColor c squaredDeltaEab


-- generate the 216 colours of ANSI 256 colors palette
ansiPalette216 :: [(Integer, Shade)]
ansiPalette216 = zip ids shades
 where
  ids    = iterate succ 16
  shades = [ sRGB24 r g b | r <- steps, g <- steps, b <- steps ]
  steps  = [0x0, 0x5f, 0x87, 0xaf, 0xd7, 0xff] :: [Word8]

-- Calculate euclidean square distance in sRGB space
squaredDistanceSRGB :: Shade -> Shade -> Float
squaredDistanceSRGB a b = sum
  $ map (^^ (2 :: Integer)) [br - ar, bg - ag, bb - ab]
 where
  argb         = toSRGB a
  brgb         = toSRGB b
  (ar, ag, ab) = (channelRed argb, channelGreen argb, channelBlue argb)
  (br, bg, bb) = (channelRed brgb, channelGreen brgb, channelBlue brgb)


-- Calculate weighted square distance in sRGB space,
-- cf. wikipedia https://en.wikipedia.org/wiki/Color_difference
weightedSquareDistanceSRGB :: Shade -> Shade -> Float
weightedSquareDistanceSRGB a b =
  (2 + r / 256) ** dr2 + 4 * dg2 + (2 + (255 - r) / 256) * db2
 where
  dr2          = (br - ar) ^^ (2 :: Integer)
  dg2          = (bg - ag) ^^ (2 :: Integer)
  db2          = (bb - ab) ^^ (2 :: Integer)
  argb         = toSRGB a
  brgb         = toSRGB b
  (ar, ag, ab) = (channelRed argb, channelGreen argb, channelBlue argb)
  (br, bg, bb) = (channelRed brgb, channelGreen brgb, channelBlue brgb)
  r            = (ar + br) * 0.5


-- Calculate CIE76 delta Eab
squaredDeltaEab :: Shade -> Shade -> Float
squaredDeltaEab a b = sum $ map (^^ (2 :: Integer)) [bl - al, ba - aa, bb - ab]
 where
  (al, aa, ab) = cieLABView d65 a
  (bl, ba, bb) = cieLABView d65 b


-- 
findClosestANSIColor :: Shade -> (Shade -> Shade -> Float) -> (Integer, Shade)
findClosestANSIColor shade f = findMinima
  $ map (\b -> squaredD shade b f) ansiPalette216
 where
  findMinima = snd . minimumBy (compare `on` fst)
  squaredD a (i, b) g = (g a b, (i, b))

--
ansiBold :: String
ansiBold = setSGRCode [SetConsoleIntensity BoldIntensity]

--
ansiReset :: String
ansiReset = setSGRCode []

module Data.Fractal1D where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed (Vector)
import Data.Vector.Storable (convert)
import Sound.File.Sndfile
import Sound.File.Sndfile.Buffer.Vector (toBuffer)

fractal :: (Vector Float -> Vector Float -> Vector Float) -> [Float] -> Int -> Vector Float
fractal step seed generations = (iterate step' seed') !! generations
  where seed' = V.fromList seed
        step' = step seed'

fractal' :: [Float] -> Int -> Vector Float
fractal' = fractal mutableIndexStep

simpleStep :: Vector Float -> Vector Float -> Vector Float
simpleStep seed = V.concatMap (\x -> V.map (*x) seed)

mutableStep :: Vector Float -> Vector Float -> Vector Float
mutableStep seed inp =
  let seedLen = V.length seed
      n = V.length inp
  in V.create $ do
    v <- MV.new $ n * seedLen
    (flip V.imapM_) inp $ \i x -> do
      let slice = MV.unsafeSlice (i * seedLen) seedLen v
      V.unsafeCopy slice (V.map (*x) seed)
    return v

mutableIndexStep :: Vector Float -> Vector Float -> Vector Float
mutableIndexStep seed inp =
  let seedLen = V.length seed
      n = V.length inp
  in V.create $ do
    v <- MV.new $ n * seedLen
    (flip V.imapM_) inp $ \i x -> do
      (flip V.imapM_) seed $ \j y -> do
        MV.write v (i * seedLen + j) (x * y)
    return v

generationsFromDesiredMilliseconds :: [Float] -> Int -> Int
generationsFromDesiredMilliseconds seed desiredMS =
  let n = length seed
      samples = 44.1 * fromIntegral desiredMS
  in floor (log samples / log (fromIntegral n))

writeFractalAiff :: [Float] -> Int -> FilePath -> IO ()
writeFractalAiff seed generations path = Sound.File.Sndfile.writeFile info path fract'
  >>= (putStrLn . (++ " seconds") . show . (/ 44100) . fromIntegral)
  where fract = fractal' seed generations
        samples = V.length fract
        fract' = toBuffer (convert fract)
        info = Info { frames = samples
                    , samplerate = 44100
                    , channels = 1
                    , format = fileFormat
                    , sections = 1
                    , seekable = True
                    }
        fileFormat = Format { headerFormat = HeaderFormatAiff
                            , sampleFormat = SampleFormatDouble
                            , endianFormat = EndianFile
                            }

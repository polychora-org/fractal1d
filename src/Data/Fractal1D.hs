module Data.Fractal1D where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed (Vector)

fractal :: [Float] -> Int -> Vector Float
fractal = mutableStep

fractal' :: [Float] -> Int -> Vector Float
fractal' = simpleStep

simpleStep :: [Float] -> Int -> Vector Float
simpleStep seed generations = (iterate step xs) !! generations
  where xs = V.fromList seed
        step :: Vector Float -> Vector Float
        step = V.concatMap (\x -> V.map (*x) xs)

mutableStep :: [Float] -> Int -> Vector Float
mutableStep seed generations = (iterate step seed') !! generations
  where seed' = V.fromList seed
        seedLen = V.length seed'
        step :: Vector Float -> Vector Float
        step inp = V.create $ do
          let n = V.length inp
          v <- MV.new $ n * seedLen
          (flip V.imapM_) inp $ \i x -> do
            let slice = MV.unsafeSlice (i * seedLen) seedLen v
            V.unsafeCopy slice (V.map (*x) seed')
          return v



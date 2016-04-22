module Main where

import Criterion.Main
import Data.Fractal1D

seed :: [Float]
seed = [1.0, 0.75.. -1.0]

generations :: Int
generations = 7

main :: IO ()
main = defaultMain [
  bgroup "fractal" [ bench "basic" $ whnf (fractal simpleStep seed) generations
                   , bench "mutable" $ whnf (fractal mutableStep seed) generations
                   , bench "mutableIndex" $ whnf (fractal mutableIndexStep seed) generations
                   ]
  ]

module Main where

import Criterion.Main
import Data.Fractal1D

seed :: [Float]
seed = [1.0, 0.75.. -1.0]

generations :: Int
generations = 5

main :: IO ()
main = defaultMain [
  bgroup "fractal" [ bench "basic" $ whnf (fractal seed) generations
                   , bench "mutable" $ whnf (fractal' seed) generations
                   ]
  ]

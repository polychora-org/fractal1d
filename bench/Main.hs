module Main where

import Criterion.Main
import Data.Fractal1D

main = defaultMain [
  bgroup "fractal" [ bench "basic" $ whnf fractal [0.5,-0.5]
                   -- ,
                   ]
  ]

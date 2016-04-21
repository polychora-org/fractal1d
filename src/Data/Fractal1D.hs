module Data.Fractal1D where

import qualified Data.Vector as Vector
import Data.Vector (Vector)

fractal :: [Float] -> Int -> Vector Float
fractal seed generations = (iterate step xs) !! generations
  where xs = Vector.fromList seed
        step :: Vector Float -> Vector Float
        step = Vector.concatMap (\x -> Vector.map (*x) xs)

module Data.Fractal1D where

import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed (Vector)

fractal :: [Float] -> Int -> Vector Float
fractal = simpleStep

simpleStep :: [Float] -> Int -> Vector Float
simpleStep seed generations = (iterate step xs) !! generations
  where xs = Vector.fromList seed
        step :: Vector Float -> Vector Float
        step = Vector.concatMap (\x -> Vector.map (*x) xs)

mutableStep :: [Float] -> Int -> Vector Float
mutableStep seed generations = (iterate step xs) !! generations
  where xs = Vector.fromList seed
        step :: Vector Float -> Vector Float
        step = Vector.concatMap (\x -> Vector.map (*x) xs)



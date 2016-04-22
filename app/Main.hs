module Main where

import Data.Fractal1D
import System.Environment

seed :: [Float]
seed = [1.0, -0.5]

generations :: Int
generations = generationsFromDesiredMilliseconds seed 10000

fractalFileName :: [Float] -> Int -> String
fractalFileName seed' generations' =
  "fractal" ++ show seed' ++ "_" ++ show generations' ++ ".aif"

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
  then writeFractalAiff seed generations (fractalFileName seed generations)
  else do
    let desiredMS = read (head args)
        seed' = map read (tail args)
        gens = generationsFromDesiredMilliseconds seed' desiredMS
    writeFractalAiff seed' gens (fractalFileName seed' gens)

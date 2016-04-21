module Data.Fractal1DSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Fractal1D
import Data.Vector (fromList)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fractal" $ do
    it "should return the seed after 0 generations" $ property $
      \xs -> fractal xs 0 === fromList xs
    it "should return the seed iterated once" $
      fractal [1.0, 0.5] 1 `shouldBe` fromList [1.0, 0.5, 0.5, 0.25]
    it "should return the seed iterated twice" $
      fractal [1.0, 0.5] 2 `shouldBe` fromList [ 1.0
                                               , 0.5
                                               , 0.5
                                               , 0.25
                                               , 0.5
                                               , 0.25
                                               , 0.25
                                               , 0.125
                                               ]

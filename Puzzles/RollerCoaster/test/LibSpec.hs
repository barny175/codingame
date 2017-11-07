module LibSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Array

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "splitGroups" $ do
    let groups = listArray (0, 4) [1, 3, 6, 10, 15]
        groups2 = listArray (0, 4) [1, 3, 6, 8, 12]
    it ("split " ++ (show $ elems groups) ++ " to fill 2 places") $ do
      splitGroups groups 0 2 `shouldBe` (1, 1)
    it ("split " ++ (show $ elems groups) ++ " to fill 3 places") $ do
        splitGroups groups 0 3 `shouldBe` (2, 3)
    it ("split " ++ (show $ elems groups) ++ " to fill 2 places starting from 1") $ do
        splitGroups groups 1 2 `shouldBe` (2, 2)
    it ("split " ++ (show $ elems groups) ++ " to fill 5 places starting from 1") $ do
        splitGroups groups 1 5 `shouldBe` (3, 5)
    it ("split " ++ (show $ elems groups) ++ " to fill 7 places starting from 4") $ do
            splitGroups groups 4 8 `shouldBe` (2, 8)
    it ("split " ++ (show $ elems groups2) ++ " to fill 5 places starting from 4") $ do
        splitGroups groups2 4 5 `shouldBe` (1, 5)
  ride2Spec

ride2Spec :: Spec
ride2Spec = do
    let groups = [1..5]
        groups2 = [1, 2, 3, 2, 4]
        groups3 = [3, 1 , 1, 2]
    describe "ride" $ do
        it ("counts dirhams earned for 2 places and 1 ride for groups " ++ (show $ groups)) $ do
            ride 2 1 groups `shouldBe` 1
        it ("counts dirhams earned for 2 places and 2 rides for groups " ++ (show $ groups))  $ do
            ride 2 2 groups `shouldBe` 3
        it ("counts dirhams earned for 5 places and 4 rides for groups " ++ (show $ groups2))  $ do
            ride 5 4 groups2 `shouldBe` ((1+2) + (3+2) + (4+1) + (2+3))
        it ("counts dirhams earned for 3 places and 3 rides for groups " ++ (show $ groups3))  $ do
            ride 3 3 groups3 `shouldBe` 7

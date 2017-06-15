module LibSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Array

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "selectGroups" $ do
    it "select groups to fill all places" $ do
      selectGroups [2] 2 `shouldBe` ([2], [])
    it "select groups to fill all places" $ do
        selectGroups [2, 3, 5] 3 `shouldBe` ([2], [3, 5])
    it "select groups to fill all places" $ do
        selectGroups [2, 3, 5] 30 `shouldBe` ([2, 3, 5], [])
  describe "splitGroups" $ do
    let groups = listArray (0, 4) [1..5]
    it ("split " ++ (show groups) ++ " to fill 2 places") $ do
      splitGroups groups 0 2 `shouldBe` (1, 1)
    it ("split " ++ (show groups) ++ " to fill 3 places") $ do
        splitGroups groups 0 3 `shouldBe` (2, 3)
    it ("split " ++ (show groups) ++ " to fill 2 places starting from 1") $ do
        splitGroups groups 1 2 `shouldBe` (1, 2)
    it ("split " ++ (show groups) ++ " to fill 5 places starting from 1") $ do
        splitGroups groups 1 5 `shouldBe` (2, 5)
    it ("split " ++ (show groups) ++ " to fill 7 places starting from 4") $ do
            splitGroups groups 4 7 `shouldBe` (3, 8)
    ride2Spec

ride2Spec :: Spec
ride2Spec = do
    let groups = listArray (0, 4) [1..5]
    describe "ride2" $ do
        it "counts dirhams earned" $ do
            ride2 2 1 groups 0 15 `shouldBe` 1
        it "counts dirhams earned" $ do
            ride2 2 2 groups 0 15 `shouldBe` 3

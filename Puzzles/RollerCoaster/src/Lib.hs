module Lib (selectGroups, ride, splitGroups, ride2) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.Array

splitGroups :: Array Int Int -> Int -> Int -> (Int, Int)
splitGroups ga start places =
    let gaSize = (snd $ bounds ga) - (fst $ bounds ga) + 1
        arrayElem i = ga ! ((start + i - 1) `mod` gaSize)
        sums = scanl1 (+) $ map arrayElem [1..gaSize]
        taken = length $ filter (<=places) sums
    in (if start + taken > gaSize then taken - (gaSize - start) else taken + start,
        if taken > 0 then sums !! (taken - 1) else 0)

-- rideW :: Int -> Int -> Array Int Int -> Int -> Writer String Int
ride2 places rides groups start
    | rides == 0 = 0
    | people < places = rides * people
    | otherwise = sum' + (ride2 places (rides - 1) groups takenGroups)
    where people = sum groups
          (takenGroups, sum') = splitGroups groups start places


ride places rides groups people
    | rides == 0 = 0
    | people < places = people + (ride places (rides - 1) groups people)
    | otherwise = dirhams
    where (sg, rest) = selectGroups groups places
          dirhams = sum sg + (ride places (rides - 1) (rest ++ sg) people)

-- selectGroups :: [[Int]] -> Int -> ([Int], [Int])
selectGroups [] _ = ([], [])
selectGroups groups@(g:gs) places
  | places == 0 || g > places= ([], groups)
  | g <= places = (g:sg, rest)
  where (sg,rest) = selectGroups gs (places - g)

module Lib (selectGroups, ride, splitGroups, ride2) where

import Control.Monad
import Control.Monad.State

import Data.Array

-- selectGroups :: [[Int]] -> Int -> ([Int], [Int])
selectGroups [] _ = ([], [])
selectGroups groups@(g:gs) places
    | places == 0 || g > places= ([], groups)
    | g <= places = (g:sg, rest)
    where (sg,rest) = selectGroups gs (places - g)

splitGroups :: Array Int Int -> Int -> Int -> (Int, Int)
splitGroups ga start places =
    let waitingGroupCount = min (places - start) ((snd $ bounds ga) - start)
        sumA n = sum $ map (\i -> ga ! i) (take n [start..])
        sums = map sumA [1..waitingGroupCount]
        taken = length $ filter (<=places) sums
    in (taken, if taken > 0 then sums !! (taken - 1) else 0)
--
ride2 places rides groups start people
    | rides == 0 = 0
    | people < places = rides * people
    | otherwise = dirhams
    where (takenGroups, sum') = splitGroups groups start places
          rest = (start + takenGroups) `mod` places
          dirhams = sum' + (ride2 places (rides - 1) groups rest people)

ride places rides groups people
    | rides == 0 = 0
    | people < places = people + (ride places (rides - 1) groups people)
    | otherwise = dirhams
    where (sg, rest) = selectGroups groups places
          dirhams = sum sg + (ride places (rides - 1) (rest ++ sg) people)

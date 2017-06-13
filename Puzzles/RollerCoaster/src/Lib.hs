module Lib (selectGroups, ride, splitGroups) where

import Control.Monad
import Control.Monad.State

import Data.Array

-- selectGroups :: [[Int]] -> Int -> ([Int], [Int])
selectGroups [] _ = ([], [])
selectGroups groups@(g:gs) places
    | places == 0 || g > places= ([], groups)
    | g <= places = (g:sg, rest)
    where (sg,rest) = selectGroups gs (places - g)

splitGroups :: Array Int Int -> Int -> Int -> Int
splitGroups ga start places =
    let waitingGroupCount = min (places - start) ((snd $ bounds ga) - start)
        sumA n = sum $ map (\i -> ga ! i) (take n [start..])
    in length . filter (<=places) $ map sumA [1..waitingGroupCount]

ride places rides groups people
    | rides == 0 = 0
    | people < places = people + (ride places (rides - 1) groups people)
    | otherwise = dirhams
    where (sg, rest) = selectGroups groups places
          dirhams = sum sg + (ride places (rides - 1) (rest ++ sg) people)

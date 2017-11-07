module Lib (ride, splitGroups, ride2, sumsElem) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Array

asize a = (snd $ bounds a) - (fst $ bounds a) + 1

sumsElem :: Array Int Int -> Int -> Int -> Int
sumsElem groupSums 0 i = groupSums ! (i - 1)
sumsElem groupSums start i
    | pos < size = groupSums ! pos - groupSums ! (start - 1)
    | pos >= size = groupSums ! (pos - size) + groupSums ! (size - 1) - groupSums ! (start - 1)
    where size = asize groupSums
          pos = start + i - 1

splitGroups :: Array Int Int -> Int -> Int -> (Int, Int)
splitGroups groupSums start places =
    let gaSize = asize groupSums
        fits = filter (<= places) $ map (\i -> sumsElem groupSums start i) [1..gaSize]
        taken = length fits
    in (if start + taken > gaSize then taken - (gaSize - start) else taken + start,
        if taken > 0 then last fits else 0)


ride :: Int -> Int -> [Int] -> Int
ride places' rides groups
    | people < places' = rides * people
    | otherwise = runReader (ride2 rides 0 0) rc
    where people = groupSums' ! (asize groupSums' - 1)
          groupSums' = listArray (0, length groups - 1) $ scanl1 (+) groups
          rc = RollerCoaster {places = places' , totalRides = rides, groupSums = groupSums' }

data RollerCoaster = RollerCoaster { places :: Int, totalRides :: Int, groupSums :: Array Int Int }

ride2 :: Int -> Int -> Int -> Reader RollerCoaster Int
ride2 rides start dirhams
    | rides == 0 = return dirhams
    | otherwise = do
        totalRides' <- asks totalRides
        places' <- asks places
        groupSums' <- asks groupSums
        config <- ask
        if (start == 0 && dirhams > 0 && (rides < totalRides' - rides)) then
            let (d, m) = totalRides' `divMod` (totalRides' - rides)
                result = runReader (ride2 m 0 (dirhams * d)) config
            in return result
        else
            let (takenGroups, sum') = splitGroups groupSums' start places'
                dirhams' = sum' + dirhams
                result = runReader (ride2 (rides - 1) takenGroups dirhams') config
            in return result


module Lib (ride, splitGroups, ride2, sumsElem, RollerCoaster(..)) where

import Control.Monad
import Data.Array
import qualified Data.Map as M
import Data.Maybe

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
    | otherwise = res
    where people = groupSums' ! (asize groupSums' - 1)
          groupSums' = listArray (0, length groups - 1) $ scanl1 (+) groups
          rc = RollerCoaster {places = places' , groupSums = groupSums' }
          res = ride2 rides 0 0 rc M.empty

data RollerCoaster = RollerCoaster { places :: Int, groupSums :: Array Int Int }
                        deriving Show

ride2 :: Int -> Int -> Int -> RollerCoaster -> (M.Map Int (Int, Int)) -> Int
ride2 rides start dirhams config rideToDirhamsMap
    | rides == 0 = dirhams
    | M.member start  rideToDirhamsMap =
        let startLkp = M.lookup start rideToDirhamsMap
            Just (prevRides, prevDirhams) = startLkp
            (d, m) = rides `divMod` (prevRides - rides)
            earnedDirhams = dirhams - prevDirhams
        in ride2 m start (earnedDirhams * d + dirhams) config M.empty
    | otherwise = let (takenGroups, sum') = splitGroups (groupSums config) start (places config)
                      newRideToDirhamsMap = M.insert start (rides, dirhams) rideToDirhamsMap
                  in ride2 (rides - 1) takenGroups (sum' + dirhams) config newRideToDirhamsMap


module Lib (ride, splitGroups, ride2, sumsElem, RollerCoaster(..)) where

import Control.Monad
import Control.Monad.RWS
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
          (res, _, _) = runRWS (ride2 rides 0 0) rc M.empty

data RollerCoaster = RollerCoaster { places :: Int, groupSums :: Array Int Int }
                        deriving Show

ride2 :: Int -> Int -> Int -> RWS RollerCoaster [String] (M.Map Int (Int, Int)) Int
ride2 rides start dirhams
    | rides == 0 = return dirhams
    | otherwise = do
        places' <- asks places
        groupSums' <- asks groupSums
        config <- ask
        rideToDirhamsMap <- get
        let startLkp = M.lookup start rideToDirhamsMap
            newRideToDirhamsMap = M.insert start (rides, dirhams) rideToDirhamsMap
        tell ["rides: " ++ (show rides) ++ " start: " ++ (show start) ++ " dirhams: " ++ (show dirhams) ++ " r2dMap: " ++ (show rideToDirhamsMap) ++ " found " ++ (show startLkp)]
        if isJust startLkp then do
            let Just (prevRides, prevDirhams) = startLkp
                (d, m) = rides `divMod` (prevRides - rides)
                earnedDirhams = dirhams - prevDirhams
                (result, _, w) = runRWS (ride2 m start (earnedDirhams * d + dirhams)) config M.empty
            tell w
            return result
        else do
            let (takenGroups, sum') = splitGroups groupSums' start places'
                dirhams' = sum' + dirhams
                (result, _, w) = runRWS (ride2 (rides - 1) takenGroups dirhams') config newRideToDirhamsMap
            tell w
            return result

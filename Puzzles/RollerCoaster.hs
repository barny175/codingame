import System.IO
import Control.Monad
import Control.Monad.State
import Data.Time.Clock

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    input_line <- getLine
    let input = words input_line
    let l = read (input!!0) :: Int
    let c = read (input!!1) :: Int
    let n = read (input!!2) :: Int
    
    groups <- replicateM n $ do
        input_line <- getLine
        let pi = read input_line :: Int
        return (pi)
    
    -- hPutStrLn stderr $ "places: " ++ (show l) ++ " people: " ++ (show $ sum groups)
    let sg = selectGroups groups l
    -- hPutStrLn stderr $ show $ (sum $ fst sg) + (head $ snd sg)
    ct <- getCurrentTime
    hPutStrLn stderr $ show ct
    putStrLn $ show $ ride l c groups
    ct <- getCurrentTime
    hPutStrLn stderr $ show ct

selectGroups :: [Int] -> Int -> ([Int], [Int])
selectGroups [] _ = ([], [])    
selectGroups gs@(g:_) places
    | g > places = ([], gs)
    | selSum == places = (sel, drop pivot gs)
    | selSum < places = let (sel', rest) = selectGroups (drop pivot gs) (places - selSum) in (sel ++ sel', rest)  
    | selSum > places = let (sel', rest) = selectGroups sel places in (sel', drop (length sel') gs)
    where len = length gs
          pivot = len `div` 2
          sel = take pivot gs
          selSum = sum sel

ride :: Int -> Int -> [Int] -> Int 
ride places rides groups 
    | rides == 0 = 0
    | people < places = people + (ride places (rides - 1) groups)
    | otherwise = dirhams
    where people = sum groups
          (sg, rest) = selectGroups groups places
          dirhams = sum sg + (ride places (rides - 1) (rest ++ sg))